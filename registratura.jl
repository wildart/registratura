#!/usr/bin/env julia

# active project directory
import Pkg
Pkg.activate(dirname(@__FILE__))

# load modules
using ArgParse
import Pkg.TOML
import LibGit2
import UUIDs
import Pkg.Types: VersionRange, VersionBound

const NAME = "name"
const UUID = "uuid"
const REPO = "repo"
const DEPS = "deps"
const PKGS = "packages"
const COMPAT = "compat"
const PROJECT_FILE  = "Project.toml"
const REGISTRY_FILE = "Registry.toml"
const REGISTRATURA_FILE  = "Registratura.toml"
const USER_DEPO = first(Pkg.depots())

function isrepo(pkgdir::String)
    try
        repo = LibGit2.GitRepo(pkgdir)
        close(repo)
    catch
        return false
    end
    return true
end

function readregistry(regpath::String)
    regfile = joinpath(regpath, REGISTRY_FILE)
    !isfile(regfile) && error("Registry configuration is not found in $regpath")
    reg = TOML.parsefile(regfile)
    registrafile = joinpath(regpath, REGISTRATURA_FILE)
    registra = isfile(registrafile) ? TOML.parsefile(registrafile) : nothing
    return TOML.parsefile(regfile), registra
end

function readproject(pkgdir::String)
    prjfile = joinpath(pkgdir, PROJECT_FILE)
    !isfile(prjfile) && error("Package must have `Project.toml` file")
    return TOML.parsefile(prjfile)
end

function genpackage(pkgdir::String, project::Dict{T,Any}) where T<:AbstractString
    package = Dict{T, T}()
    package[NAME] = if haskey(project, NAME)
        project[NAME]
    else
        basename(PKGSRC)
    end
    package[UUID] = if haskey(project, UUID)
        project[UUID]
    else
        string(UUIDs.uuid1())
    end
    package["repo"] = LibGit2.with(LibGit2.GitRepo(pkgdir)) do repo
        LibGit2.with(LibGit2.get(LibGit2.GitRemote, repo, "origin")) do remote
            LibGit2.url(remote)
        end
    end
    return package
end

function genversions(pkgdir)
    versions = Dict{String, Any}()
    LibGit2.with(LibGit2.GitRepo(pkgdir)) do repo
        tags = LibGit2.tag_list(repo)
        for tag in tags
            tagref = LibGit2.GitReference(repo, "refs/tags/$tag")
            try
                v = string(VersionNumber(tag))
                versions[v] = Dict("git-tree-sha1"=>string(LibGit2.GitHash(tagref)))
            catch
                @debug "Skip" tag=tag
            end
        end
    end
    return versions
end

function group(A::Vector{T}, sentinel::T=last(A)) where T
    p = sortperm(A)
    q = A[p]
    res = Vector{Vector{Int}}()
    grp = Vector{Int}()
    first = true
    last = sentinel
    for (i,v) in enumerate(q)
        if !first && last != v
            push!(res, grp)
            grp = [p[i]]
        else
            push!(grp, p[i])
        end
        last = v
        first = false
    end
    push!(res, grp)
    return res
end

"""Generate dependencies from `REQUIRE` file"""
function gendependenciesreq(pkgdir::String, versions::Dict{T,Any}) where T<:AbstractString
    # read all dependencies from repo
    depvers = Dict{T,Vector{VersionNumber}}()
    verhash = [k => versions[k]["git-tree-sha1"] for k in sort(collect(keys(versions)))]
    LibGit2.with(LibGit2.GitRepo(pkgdir)) do repo
        for (ver, hash) in verhash
            LibGit2.with(LibGit2.GitTree(repo, "$hash^{tree}")) do tree
                cont = LibGit2.content(tree["REQUIRE"])
                for req in split(cont, '\n', keepempty=false)
                    dep = first(split(req, ' '))
                    #dep == "julia" && continue
                    !haskey(depvers, dep) && setindex!(depvers, VersionNumber[], dep)
                    push!(depvers[dep], VersionNumber(ver))
                end
            end
        end
    end

    # aggregate dependencies by versions
    deps = Dict{T,Dict{T,T}}()

    # get registry context
    ctx = Pkg.Types.Context()
    Pkg.Types.Context!(ctx)

    # group dependencies by min-max versions
    verdeps = [extrema(vers) => dep for (dep, vers) in depvers if dep != "julia"]
    gidxs = group(map(first, verdeps))
    for g in gidxs
        # find uuids of packages
        pkgspecs = map(Pkg.Types.PackageSpec, map(last, verdeps[g]))
        Pkg.Types.project_deps_resolve!(ctx.env, pkgspecs)
        Pkg.Types.registry_resolve!(ctx.env, pkgspecs)
        # form version range
        vv = first(first(verdeps[g]))
        vrange = VersionRange(map(VersionBound, vv)...) |> string
        deps[vrange] = Dict{T,T}()
        for ps in pkgspecs
            deps[vrange][ps.name] = string(ps.uuid)
        end
    end

    return deps, depvers
end

function gendependencies(pkgdir::String, versions::Dict{T,Any}) where T<:AbstractString
    # read all dependencies from repo
    depvers = Dict{T,Any}()
    verhash = [k => versions[k]["git-tree-sha1"] for k in sort(collect(keys(versions)))]
    LibGit2.with(LibGit2.GitRepo(pkgdir)) do repo
        for (ver, hash) in verhash
            LibGit2.with(LibGit2.GitTree(repo, "$hash^{tree}")) do tree
                try
                    cont = LibGit2.content(tree["Project.toml"])
                    prj = TOML.parse(cont)
                    depvers[ver] = prj[DEPS]
                catch
                    @debug "No project" ver hash
                end
            end
        end
    end
    return depvers
end

function aggregateverdeps(verdeps)
    # aggregate dependencies by versions
    cdeps = Dict{String, Any}()

    # group dependencies by min-max versions
    gidxs = group(map(first, verdeps))
    for g in gidxs
        vv = first(first(verdeps[g])) # form version range
        vrange = VersionRange(map(VersionBound, vv)...) |> string
        cdeps[vrange] = Dict{String,String}()
        for i in g
            pkg = last(verdeps[i])
            cdeps[vrange][first(pkg)] = last(pkg)
        end
    end
    return cdeps
end

function compactdeps(deps::Dict{T,Any}) where T<:AbstractString
    alldeps = vcat(([ (d,uuid) for (d,uuid) in ds] for (v, ds) in deps)...) |> unique
    depvers = [ d => [ VersionNumber(k) for (k, depid) in deps if haskey(depid, first(d))] for d in alldeps]
    verdeps = [extrema(vers) => dep for (dep, vers) in depvers]
    return aggregateverdeps(verdeps)
end

function gencompatibility(pkgdir::String, versions::Dict{T,Any}) where T<:AbstractString
    # read all dependencies from repo
    compatvers = Dict{T,Any}()
    verhash = [k => versions[k]["git-tree-sha1"] for k in sort(collect(keys(versions)))]
    LibGit2.with(LibGit2.GitRepo(pkgdir)) do repo
        for (ver, hash) in verhash
            # println(ver => hash)
            LibGit2.with(LibGit2.GitTree(repo, "$hash^{tree}")) do tree
                try
                    prjfile = LibGit2.content(tree["Project.toml"])
                    prj = TOML.parse(prjfile)
                    compatvers[ver] = prj[COMPAT]
                catch
                    @debug "No project" ver hash
                end
            end
        end
    end
    return compatvers
end

function compactcompats(compats::Dict{T,Any}) where T<:AbstractString
    cptverschange = vcat(([dver => v for dver in dvers] for (v, dvers) in compats)...)
    length(cptverschange) == 0 && return Dict{String,Any}() # no compat info
    gidxs = group(map(first, cptverschange))
    vercpts = [extrema(map(p->VersionNumber(last(p)), cptverschange[g])) => first(cptverschange[first(g)]) for g in gidxs]

    return aggregateverdeps(vercpts)
end


function parse_commandline()
    s = ArgParseSettings()

    @add_arg_table s begin
        "init"
            help = "initialize a registry"
            action = :command
        "update"
            help = "update a registry"
            action = :command
        "add"
            help = "add a package to a registry"
            action = :command
    end

    @add_arg_table s["init"] begin
        "path"
            arg_type = String
            help = "a registry location"
            required = true
    end

    @add_arg_table s["add"] begin
        "registry"
            arg_type = String
            help = "a registry location"
            required = true
        "pkg"
            arg_type = String
            help = "a package location"
            required = true
    end

    @add_arg_table s["update"] begin
        "registry"
            arg_type = String
            help = "a registry location"
            required = true
    end

    return parse_args(s)
end

isregistrydir(path::String) = isdir(path) && isrepo(path) && isfile(joinpath(path, REGISTRY_FILE))

function getregistrypath(path::String)
    if isdirpath(path)
        return path, ""
    else
        return joinpath(USER_DEPO, "registries", path), path
    end
end

function writeto(f, fpath)
    io = open(fpath, "w")
    try
        f(io)
    finally
        close(io)
    end
end

saveregistryfile(path, reg) = writeto(io->TOML.print(io, reg), path)

function init(path::String)
    regpath, regname = getregistrypath(path)

    if !isregistrydir(regpath)
        # input registry properties
        reg = Dict{String, Any}()
        reg[UUID] = UUIDs.uuid4()
        print("Enter registry name$(isempty(regname) ? ":" : " ["*regname*"]:") ")
        tmpname = readline(stdin)
        reg[NAME] = isempty(tmpname) ? regname : tmpname
        print("Enter registry repository url: ")
        reg[REPO] = readline(stdin)
        reg[PKGS] = Dict{String,Any}()
        print("Enter registry description: ")
        reg["description"] = readline(stdin)
        print("Create registry [Y/n]: ")
        readline(stdin) == "n" && return

        # create directory
        mkpath(regpath)
        # create repo
        repo = LibGit2.init(regpath)
        try
            # generate Registry.toml
            regfilepath = joinpath(regpath, REGISTRY_FILE)
            saveregistryfile(regfilepath, reg)

            LibGit2.add!(repo, REGISTRY_FILE)
            LibGit2.commit(repo, "Registry created.")
            rmt = LibGit2.GitRemote(repo, "origin", reg[REPO])
            try
                LibGit2.add_push!(repo, rmt, "refs/heads/master")
                LibGit2.add_fetch!(repo, rmt, "refs/heads/master")
            finally
                close(rmt)
            end
            LibGit2.set_remote_url(repo, "origin", reg[REPO])

            println("Registry is created in $regpath")
        catch ex
            rm(regpath, recursive=true, force=true)
            rethrow(ex)
        finally
            close(repo)
        end
    else
        error("Registry is already created in $path.")
    end
end

function addjuliavers!(compats, vers)
    svers = sort!(collect(keys(vers)))
    # if no compatibility information exists
    # set julia compatibility versin using the current version of julia compiler
    lastver = svers[end]
    if !haskey(compats, lastver)
        compats[lastver] = Dict{String,Any}()
    end
    if !haskey(compats[lastver], "julia")
        compats[lastver]["julia"] = string(VERSION)
    end
end

function addpkg(regdir::String, pkgdir::String)
    regpath, regname = getregistrypath(regdir)

    # load registry & package config
    reg, registra = readregistry(regpath)
    prj = readproject(pkgdir)
    prjname = prj[NAME]
    prjid = prj[UUID]

    # get package versions
    vers = genversions(pkgdir)
    if length(vers) == 0
        @info "Package $prjname doesn't have versions."
        return
    end

    # create registry record
    prjpath = joinpath(regpath, prjname)
    isdir(prjpath) && error("Package `$(prjname)` is already added to the registry `$(reg[NAME])`")
    mkpath(prjpath)

    # write package description
    writeto(joinpath(prjpath, "Package.toml")) do io
        prjdesc = Dict{String, Any}()
        prjdesc[NAME] = prjname
        prjdesc[UUID] = prjid

        repo = LibGit2.GitRepo(pkgdir)
        try
            rmt = LibGit2.lookup_remote(repo, "origin")
            repourl = LibGit2.url(rmt)
            close(rmt)
            if startswith(repourl, "git") # convert to http
                repourl = replace(repourl, ":" =>"/")
                repourl = replace(repourl, "git@" =>"https://")
            end
            print("Confirm package remote repository location [$repourl]: ")
            tmpurl = readline(stdin)
            prjdesc[REPO] = isempty(tmpurl) ? repourl : tmpurl
        catch err
            rethrow(err)
        finally
            close(repo)
        end
        TOML.print(io, prjdesc)
    end

    # write package versions
    writeto(joinpath(prjpath, "Versions.toml")) do io
        TOML.print(io, vers)
    end

    # write compatibility reqs
    writeto(joinpath(prjpath, "Compat.toml")) do io
        compats = gencompatibility(pkgdir, vers)
        addjuliavers!(compats, vers)
        TOML.print(io, compactcompats(compats))
    end

    # write dependencies
    writeto(joinpath(prjpath, "Deps.toml")) do io
        deps = gendependencies(pkgdir, vers)
        TOML.print(io, compactdeps(deps))
    end

    # generate record in a Registry.toml
    if !haskey(reg, PKGS)
        reg[PKGS] = Dict{String, Any}()
    end
    reg[PKGS][prjid] = Dict{String, Any}()
    reg[PKGS][prjid]["name"] = prjname
    reg[PKGS][prjid]["path"] = prjname
    saveregistryfile(joinpath(regpath, REGISTRY_FILE), reg)

    # write package record to Registratura config
    writeto(joinpath(regpath, REGISTRATURA_FILE)) do io
        if registra === nothing
            registra = Dict{String,String}()
        end
        if !haskey(registra, prjid)
            registra[prjid] = abspath(pkgdir)
            TOML.print(io, registra)
        end
    end

    # commit changes
    repo = LibGit2.init(regpath)
    try
        LibGit2.add!(repo, prjname)
        LibGit2.add!(repo, REGISTRY_FILE)
        LibGit2.commit(repo, "Added package $prjname to the registry.")
    finally
        close(repo)
    end

    println("$prjname added to registry in $regpath")
end

function main()
    args = parse_commandline()
    # println("Parsed args:")
    # for (arg,val) in args
    #     println("  $arg  =>  $val")
    # end

    # process commands
    cmd = args["%COMMAND%"]
    if cmd == "init"
        init(args["init"]["path"])
    elseif cmd == "add"
        addargs = args["add"]
        addpkg(addargs["registry"], addargs["pkg"])
    else
        error("Unknow command `$cmd`")
    end
end

!isinteractive() && main()

