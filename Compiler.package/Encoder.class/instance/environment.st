encoding
environment
	"Answer the environment of the current compilation context,
	 be it in a class or global (e.g. a workspace)"
	^class == nil
		ifTrue: [Smalltalk globals]
		ifFalse: [class environment]