run:
	json-server --watch theory-db.json -p 5019 &
	elm-live src/Main.elm --open --pushstate -- --output=elm.js --debug