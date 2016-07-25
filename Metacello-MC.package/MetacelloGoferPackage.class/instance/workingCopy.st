accessing
workingCopy
	
	workingCopy == nil ifTrue: [ workingCopy := self findWorkingCopy ].
	^workingCopy