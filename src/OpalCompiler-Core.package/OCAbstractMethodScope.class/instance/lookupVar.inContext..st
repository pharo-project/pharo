lookup
lookupVar: name inContext: aContext
	"We lookup a variable in a context. If it not in this context, we look in the outer context using the corresponding outer scope"
	
	| variable theValue outerContext |
	variable := self
		variableNamed: name
		ifAbsent: [ 
			"if we ask for a method, not a block context, do not ask for outerContext"
			outerContext := aContext outerContext ifNil: [ aContext ].
			^self outerScope lookupVar: name inContext: outerContext ].
	theValue := variable readFromContext: aContext scope: self.
	^ theValue