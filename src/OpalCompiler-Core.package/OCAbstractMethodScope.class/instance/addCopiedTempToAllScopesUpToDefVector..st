temp vars
addCopiedTempToAllScopesUpToDefVector: aVariable
		
	(self hasCopyingTempNamed: aVariable name)
		ifFalse: [self addCopyingTemp: aVariable ].
	self tempVectorName = aVariable name ifTrue: [^ self].
	^ self outerScope addCopiedTempToAllScopesUpToDefVector: aVariable.