converting
asLiteralIn: env
	^ value collect: [ :e | e asLiteralIn: env ]