import invoke


@invoke.task
def build(context):
    context.run("elm make src/Main.elm --output=main.js")
