#!/usr/bin/env julia

# active project directory
import Pkg
Pkg.activate(dirname(@__FILE__))

# load modules
using ArgParse
import Pkg.TOML
import LibGit2
import UUIDs
import Pkg.Resolve.VersionWeights: VersionWeight

const NAME = "name"
const UUID = "uuid"
const REPO = "repo"
const DEPS = "deps"
const PKGS = "packages"
const COMPAT = "compat"
const REGISTRY_FILE = "Registry.toml"

function isrepo(pkgdir::String)
    try
        repo = LibGit2.GitRepo(pkgdir)
        close(repo)
    catch
        return false
    end
    return true
end

function readproject(pkgdir::String)
    prjfile = joinpath(pkgdir, "Project.toml")
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

function genversions(pkgdir::T) where T<:AbstractString
    versions = Dict{T, Any}()
    LibGit2.with(LibGit2.GitRepo(pkgdir)) do repo
        tags = LibGit2.tag_list(repo)
        for tag in tags
            tagref = LibGit2.GitReference(repo, "refs/tags/$tag")
            try
                VersionNumber(tag)
                versions[tag] = Dict("git-tree-sha1"=>string(LibGit2.GitHash(tagref)))
            catch
                @info "Skip" tag=tag
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
            #println(ver => hash)
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
        vrange = Pkg.Types.VersionRange(map(Pkg.Types.VersionBound, vv)...) |> string
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
            #println(ver => hash)
            LibGit2.with(LibGit2.GitTree(repo, "$hash^{tree}")) do tree
                try
                    cont = LibGit2.content(tree["Project.toml"])
                    prj = TOML.parse(cont)
                    depvers[ver] = prj[DEPS]
                catch
                    @info "No project" ver hash
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
        vrange = Pkg.Types.VersionRange(map(Pkg.Types.VersionBound, vv)...) |> string
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
            #println(ver => hash)
            LibGit2.with(LibGit2.GitTree(repo, "$hash^{tree}")) do tree
                try
                    cont = LibGit2.content(tree["Project.toml"])
                    prj = TOML.parse(cont)
                    compatvers[ver] = prj[COMPAT]
                catch
                    @info "No project" ver hash
                end
            end
        end
    end
    return compatvers
end


function compactcompats(compats::Dict{T,Any}) where T<:AbstractString
    #allcpts = vcat((collect(keys(dvers)) for (v, dvers) in compats)...) |> unique
    #cptvers = sort([VersionNumber(v) for v in keys(compats)])
    #alldeps = vcat(([ (d,uuid) for (d,uuid) in ds] for (v, ds) in compats)...) |> unique
    #allcompats = vcat(([ (d,uuid) for (d,uuid) in ds] for (v, ds) in compats)...) |> unique
    #compatvers = [ d => [ VersionNumber(k) for (k, depid) in deps if haskey(depid, first(d))] for d in allcompats]
    #verdeps = [extrema(vers) => dep for (dep, vers) in depvers]

    cptverschange = vcat(([dver => v for dver in dvers] for (v, dvers) in compats)...)
    length(cptverschange) == 0 && return Dict{String,Any}() # no compat info
    gidxs = group(map(first, cptverschange))
    vercpts = [extrema(map(p->VersionNumber(last(p)), cptverschange[g])) => first(cptverschange[first(g)])  for g in gidxs]

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
        return joinpath(first(Pkg.depots()), "registries", path), path
    end
end

saveregistryfile(path, reg) = open(io->TOML.print(io, reg), path, "w")

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

function addpkg(regdir::String, pkgdir::String)
    regpath, regname = getregistrypath(regdir)
    regfilepath = joinpath(regpath, REGISTRY_FILE)
    reg = TOML.parsefile(regfilepath)
    prj = readproject(pkgdir)

    # create registry record
    prjpath = joinpath(regpath, prj[NAME])
    isdir(prjpath) && error("Project $(prj[NAME]) is already added to the registry $(reg[NAME])")
    mkpath(prjpath)

    # write package versions
    vers = genversions(pkgdir)
    open(joinpath(prjpath, "Versions.toml"), "w") do io
        TOML.print(io, vers)
    end

    # write package description
    open(joinpath(prjpath, "Package.toml"), "w") do io
        prjdesc = Dict{String, Any}()
        prjdesc[NAME] = prj[NAME]
        prjdesc[UUID] = prj[UUID]

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

    # write compatibility reqs
    open(joinpath(prjpath, "Compat.toml"), "w") do io
        compats = gencompatibility(pkgdir, vers)
        TOML.print(io, compactcompats(compats))
    end

    # write dependencies
    open(joinpath(prjpath, "Deps.toml"), "w") do io
        deps = gendependencies(pkgdir, vers)
        TOML.print(io, compactdeps(deps))
    end

    # generate record in a Registry.toml
    if !haskey(reg, PKGS)
        reg[PKGS] = Dict{String, Any}()
    end
    reg[PKGS][prj[UUID]] = Dict{String, Any}()
    reg[PKGS][prj[UUID]]["name"] = prj[NAME]
    reg[PKGS][prj[UUID]]["path"] = prj[NAME]
    TOML.print(reg)
    saveregistryfile(regfilepath, reg)
end

function main()
    args = parse_commandline()
    println("Parsed args:")
    for (arg,val) in args
        println("  $arg  =>  $val")
    end

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

main()

