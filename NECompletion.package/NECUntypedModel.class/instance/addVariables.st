private
addVariables
	includeVariables
		ifFalse: [^ self].
	self addToEntries: localVars.
	self addToEntries: instVars.
	self addToEntries: classVars