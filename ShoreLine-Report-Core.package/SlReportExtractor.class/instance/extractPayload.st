extraction
extractPayload

	^ (debugger session stack collect: [ :e | e asString ]) asArray