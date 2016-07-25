running
verifyRenameEvent: aRenamedEvent
	| renamedClass |
	renamedClass := aRenamedEvent classRenamed.
	self assert: (Smalltalk globals classNamed: newClassName) name = newClassName.
	self assert: renamedClass name = newClassName