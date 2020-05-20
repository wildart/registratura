#!/usr/bin/env julia

# load modules
using Pkg
using Pkg.TOML: TOML
using LibGit2: LibGit2
using UUIDs: uuid1, uuid4
using Pkg.Types: VersionRange, VersionBound

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
const COMMANDS = Dict("init" => ["path"], "update"  => ["registry"], "add"  => ["registry", "pkg"])
const OPTS = Dict("dry-run" => false, "help" => false)


function usage(err; cmd="")
    if length(err) > 0
        println(err)
    end
    if cmd == "" || cmd == "help"
"""
usage: registratura.jl [--dry-run] [--help] {init|update|add}
""" |> print
    end
    if cmd == "help"
"""

commands:
  init        initialize a registry
  update      update a registry
  add         add a package to a registry

""" |> print
    end
    if cmd == "init"
"""
usage: registratura.jl init [--help] path

positional arguments:
  path        a registry location

""" |> print
    end
    if cmd == "add"
"""
usage: registratura.jl add [--help] registry pkg

positional arguments:
  registry    a registry location
  pkg         a package location

""" |> print
    end
    if cmd == "update"
"""
usage: registratura.jl update [--help] registry

positional arguments:
  registry    a registry location

""" |> print
    end

    args = """
optional arguments:
  --help      show this help message and exit
"""
    if cmd == "help"
        args *= "  --dry-run   perform dry run\n"
    end
    cmd != "" && println(args)
    return
end

function parse_commandline(args)
    # setup default values
    argcmd = Dict{String,Any}()
    for (o,v) in OPTS
        argcmd[o] =v
    end
    errmsg = ""
    cnt = 1
    for arg in args
        # parse options
        if startswith(arg, "--")
            if haskey(OPTS, arg[3:end])
                argcmd[arg[3:end]] = true
            else
                errmsg = "unknown option: $arg"
                break
            end
        else
            cmd = findfirst(isequal(arg), collect(keys(COMMANDS)))
            if cmd !== nothing
                argcmd[arg] = Dict{String,Any}()
            else
                errmsg = "unknown command: $arg"
            end
            break
        end
        cnt += 1
    end
    # process command params
    cmd = collect(symdiff(keys(argcmd), keys(OPTS)))
    cmdid = ""
    if length(cmd) > 0
        cmdid = first(cmd)
        argcmd[cmdid] = Dict{String,String}()
        # add parameters to the command
        pcnt = 1
        cparams = COMMANDS[cmdid]
        lparams = length(cparams)
        for prm in args[cnt+1:end]
            if prm == "--help"
                argcmd["help"] = true
                continue
            end
            if pcnt > lparams
                errmsg = "incorrect number of parametes: $(join(args[cnt+1:end], ", "))"
                break
            end
            argcmd[cmdid][cparams[pcnt]] = prm
            pcnt += 1
        end
        # check parameters
        !argcmd["help"] && for p in COMMANDS[cmdid]
            if !haskey(argcmd[cmdid], p)
                if length(errmsg) == 0
                    errmsg = "missing parametes:"
                end
                errmsg *= " $p"
            end
        end
    end
    # println(args)
    cmdid = (length(cmdid) == 0 && argcmd["help"]) ? "help" : cmdid
    # println(argcmd, ", $cmdid, $errmsg")
    if length(cmdid) == 0 || argcmd["help"] || length(errmsg) > 0
        # println("==========================")
        usage(errmsg, cmd=cmdid)
        # println("==========================")
        exit(1)
    end
    argcmd["%COMMAND%"] = cmdid
    return argcmd
end

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
        string(uuid1())
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
            tree = LibGit2.GitTree(repo, "refs/tags/$tag^{tree}")
            try
                v = string(VersionNumber(tag))
                versions[v] = Dict("git-tree-sha1"=>string(LibGit2.GitHash(tree)))
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

isregistrydir(path::String) = isdir(path) && isrepo(path) && isfile(joinpath(path, REGISTRY_FILE))

function getregistrypath(path::String)
    if isdirpath(path)
        return path, ""
    else
        return joinpath(USER_DEPO, "registries", path), path
    end
end

function writeto(f, fpath, do_dry_run::Bool=false)
    bar = "============ $fpath ============"
    io = if do_dry_run
        println(bar)
        stdout
    else
        open(fpath, "w")
    end
    try
        f(io)
    finally
        if do_dry_run
            println("="^length(bar))
        else
            close(io)
        end
    end
end

saveregistryfile(path, reg) = writeto(io->TOML.print(io, reg), path)

function init(path::String)
    regpath, regname = getregistrypath(path)

    if !isregistrydir(regpath)
        # input registry properties
        reg = Dict{String, Any}()
        reg[UUID] = uuid4()
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
        compats[lastver]["julia"] = string(Pkg.Types.semver_spec("^$VERSION"))
    end
end

function convertvers!(compats)
    for (k,v) in compats
        for (d,ver) in v
            v[d] = string(Pkg.Types.semver_spec(ver))
        end
    end
    return compats
end

function addpkg(regdir::String, pkgdir::String, do_dry_run::Bool=false)
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
    !do_dry_run && mkpath(prjpath)

    # write package description
    writeto(joinpath(prjpath, "Package.toml"), do_dry_run) do io
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
    writeto(joinpath(prjpath, "Versions.toml"), do_dry_run) do io
        TOML.print(io, vers)
    end

    # write compatibility reqs
    writeto(joinpath(prjpath, "Compat.toml"), do_dry_run) do io
        compats = gencompatibility(pkgdir, vers)
        convertvers!(compats)
        addjuliavers!(compats, vers)
        TOML.print(io, compactcompats(compats))
    end

    # write dependencies
    writeto(joinpath(prjpath, "Deps.toml"), do_dry_run) do io
        deps = gendependencies(pkgdir, vers)
        TOML.print(io, compactdeps(deps))
    end

    if !do_dry_run
        # generate record in a Registry.toml
        if !haskey(reg, PKGS)
            reg[PKGS] = Dict{String, Any}()
        end
        reg[PKGS][prjid] = Dict{String, Any}()
        reg[PKGS][prjid]["name"] = prjname
        reg[PKGS][prjid]["path"] = prjname
        saveregistryfile(joinpath(regpath, REGISTRY_FILE), reg)

        # write package record to Registratura config
        if registra === nothing
            registra = Dict{String,String}()
        end
        if !haskey(registra, prjid)
            writeto(joinpath(regpath, REGISTRATURA_FILE)) do io
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
    end

    println("$prjname added to registry in $regpath")
end

function updreg(regdir::String, do_dry_run::Bool=false)
    regpath, regname = getregistrypath(regdir)

    # load registry & package config
    reg, registra = readregistry(regpath)

    # look for all packages in registratura
    for prjid in keys(registra)
        prjpath = registra[prjid]
        prj = readproject(prjpath)
        prjname = prj[NAME]
        println("Updating $prjname")

        if haskey(reg["packages"], prjid)
            # load version info from package and registry
            pkgpath = joinpath(regpath, prjname)
            pkgvers = TOML.parsefile(joinpath(pkgpath, "Versions.toml"))
            prjvers = genversions(prjpath)

            # if there is a version difference, begin update process
            newvers = symdiff(keys(pkgvers), keys(prjvers))
            if length(newvers) > 0
                # write down a new set of versions
                pkgvers = prjvers
                writeto(joinpath(pkgpath, "Versions.toml"), do_dry_run) do io
                    TOML.print(io, pkgvers)
                end

                # get new versions
                vers =  Dict{String,Any}(v=>prjvers[v] for v in newvers)

                # write compatibility reqs
                pkgcompats = TOML.parsefile(joinpath(pkgpath, "Compat.toml"))
                writeto(joinpath(pkgpath, "Compat.toml"), do_dry_run) do io
                    compats = gencompatibility(prjpath, vers)
                    convertvers!(compats)
                    addjuliavers!(compats, vers)
                    merge!(compats, pkgcompats)
                    TOML.print(io, compactcompats(compats))
                end

                # write dependencies
                pkgdeps = TOML.parsefile(joinpath(pkgpath, "Deps.toml"))
                writeto(joinpath(pkgpath, "Deps.toml"), do_dry_run) do io
                    deps = gendependencies(prjpath, vers)
                    merge!(deps, pkgdeps)
                    TOML.print(io, compactdeps(deps))
                end

                # commit changes
                if !do_dry_run
                    repo = LibGit2.init(regpath)
                    try
                        LibGit2.add!(repo, prjname)
                        LibGit2.commit(repo, "Updated package $prjname in the registry.")
                    finally
                        close(repo)
                    end
                end
                println("$prjname is updated in the registry $regpath")
            end
        end
    end
end

function main()
    args = parse_commandline(ARGS)
    do_dry_run = args["dry-run"]

    # process commands
    cmd = args["%COMMAND%"]
    if cmd == "init"
        init(args["init"]["path"])
    elseif cmd == "add"
        addargs = args["add"]
        addpkg(addargs["registry"], addargs["pkg"], do_dry_run)
    elseif cmd == "update"
        updreg(args["update"]["registry"], do_dry_run)
    else
        error("Unknow command `$cmd`")
    end
end

!isinteractive() && main()
