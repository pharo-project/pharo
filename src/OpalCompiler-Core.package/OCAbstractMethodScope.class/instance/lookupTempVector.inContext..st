lookup
lookupTempVector: name inContext: aContext
	"Similar to lookupVariable:inContext: but looks for a temp vector. If we don't find the temp vector (because it was nilled or not created), we lookup in the outer context with the corresponding outer scope"
	
	| variable theVector |
	variable := self
		variableNamed: name
		ifAbsent: [^ self outerScope lookupTempVector: name inContext: aContext outerContext ].
	theVector := variable readFromContext: aContext scope: self.
	theVector ifNil: [ ^ self outerScope lookupTempVector: name inContext: aContext outerContext ].
	^ { variable . theVector }