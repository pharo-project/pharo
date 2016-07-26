accessing
origin
	"Return the real oring of this method."
	^ self compiledMethod 
		ifNil: [ self methodClass ]
		ifNotNil: [ :compileMethod| compileMethod origin ]