find-replace
openFindDialog
	self sharesFindReplace 
		ifTrue: [ self announce: RubFindReplaceWindowRequired ]
		ifFalse: [self flash]