accessing
lookupVar: name inContext: aContext
	"I work as OCAbstractMethodScope>>lookupVar:inContext:, looking up in the outer scope but staying in the same context because I'm optimized"
	
	| variable theValue |
	variable := self
		variableNamed: name
		ifAbsent: [ ^ self outerScope lookupVar: name inContext: aContext ].
	theValue := variable readFromContext: aContext scope: self.
	^ theValue