import invoke


@invoke.task
def build(context):
    """ Compiles the latest elm code to main.js which is used in index.html """
    context.run(f"elm make src/Main.elm --output=public/main.js")
