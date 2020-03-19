# The Pyramid Workout Challenge
A little utility to ease
1. the 100 pushups challenge
1. the 100 pullups challenge (still working towards that)

### Set up
- check out `elm.json` for the elm dependencies
- check out `requirements.txt` for the Python dependencies (these are optional and just used for deployment)

### Deployment
A few handy shortcuts are available in `tasks.py`:
1. Use `inv build` to refresh the `main.js` file
1. Use `inv deploy` to roll out a new version to Heroku
