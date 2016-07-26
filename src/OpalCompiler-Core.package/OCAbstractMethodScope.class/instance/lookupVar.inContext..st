lookup
lookupVar: name inContext: aContext
	"We lookup a variable in a context. If it not in this context, we look in the outer context using the corresponding outer scope"
	
	| variable theValue |
	variable := self
		variableNamed: name
		ifAbsent: [ ^ self outerScope lookupVar: name inContext: aContext outerContext ].
	theValue := variable readFromContext: aContext scope: self.
	^ theValue