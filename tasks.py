import invoke


def _build(context):
    context.run(f"elm make src/Main.elm --output=main.js")


@invoke.task
def build(context):
    """ Compiles the latest elm code to main.js which is used in index.html """
    _build(context)


@invoke.task
def deploy(context):
    """
    In order to deploy to our static app to Heroku we want to use the
    heroku-buildpack-static which will serve our index.html.

    The problem here is that we'll want to run the build step (see the build
    task above) before we deploy, but we don't want to include main.js in our
    remote git repository because that would add a lot of clutter.

    The strategy of this command is to...
    1. check out a temporary branch
    2. build the main.js file
    3. move all build files to the ./build directory
    4. push the content of ./build to Heroku
    5. delete the temporary branch including the build files

    That way we make sure that the latest elm code is being deployed without
    ever pushing the main.js to "origin".
    """
    temp_branch_name = "deploy"

    # get current branch so we can return to it in the end
    res = context.run("git rev-parse --abbrev-ref HEAD", hide=True)
    previous_branch = res.stdout.strip()
    if previous_branch == temp_branch_name:
        raise ValueError(
            f"You can't deploy from branch '{temp_branch_name}'. It will be"
            " created by this command itself and deleted afterwards."
        )

    # flush the deploy branch in case it exists already
    context.run(f"git branch -D {temp_branch_name}", warn=True)
    context.run(f"git checkout -b {temp_branch_name}")

    # build
    _build(context)

    # copy everything into the /build directory
    context.run("mkdir -p build")
    directories_to_copy = ["vendor", "assets"]
    files_to_copy = ["soundManager.js", "static.json", "main.js", "index.html"]
    for name in directories_to_copy + files_to_copy:
        context.run(f"cp -a ./{name} ./build")

    # this shouldn't be necessary. but somehow is...
    context.run("cp -a ./assets/images/favicon.ico ./build")

    # add to build git
    context.run("git add --force build")
    context.run("git commit -m 'Build for deploy'")

    # now we push the /build directory as the new root to heroku
    # I don't understand the subtree magic below though
    res = context.run("git subtree split --prefix build deploy")
    res_str = res.stdout.strip()
    context.run(f"git push heroku {res_str}:refs/heads/master --force")

    # clean up
    context.run(f"git checkout {previous_branch}")
    context.run(f"git branch -D {temp_branch_name}")