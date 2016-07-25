visitor
defaultAction
	window renderer ifNotNil: [ :renderer | renderer resized ]