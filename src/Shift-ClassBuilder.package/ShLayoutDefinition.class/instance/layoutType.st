accessing
layoutType
	| layoutType |
	layoutType := builder buildEnvironment
		classNamed: self layoutClass.
	
	"If there is no layouttype in the environment. Look for in the default environment"

	layoutType
		ifNil: [ layoutType := Smalltalk at: self layoutClass ].
	^ layoutType