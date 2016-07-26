private
patchLiteralArrayToken
	(currentToken isIdentifier and: [currentToken isPatternVariable]) 
		ifTrue: [^self].
	super patchLiteralArrayToken