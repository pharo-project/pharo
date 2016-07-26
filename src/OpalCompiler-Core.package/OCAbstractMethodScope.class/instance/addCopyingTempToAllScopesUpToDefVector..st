temp vars
addCopyingTempToAllScopesUpToDefVector: aName
		
	(self hasCopyingTempNamed: aName) ifFalse: [self addCopyingTempNamed: aName].
	self tempVectorName = aName  ifTrue: [^ self].
	^ self outerScope addCopyingTempToAllScopesUpToDefVector: aName.