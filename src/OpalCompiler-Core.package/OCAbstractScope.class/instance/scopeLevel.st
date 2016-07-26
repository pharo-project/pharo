levels
scopeLevel

	outerScope ifNil: [^ 0].
	^ outerScope scopeLevel + 1