method
renameMethodFor: aMethod
	model okToChange ifFalse: [ ^ self ].
	aMethod ifNil: [ ^ self ].
	self performRefactoringFor: #privateRenameMethodFor: with: aMethod