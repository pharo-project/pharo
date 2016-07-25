*System-Support
isUsed
	self hasSubclasses ifTrue: [ ^ true ].
	^SystemNavigation new isUsedClass: self