method
renameMessageFor: aMethodOrMessageNode
	model okToChange ifFalse: [ ^ self ].
	aMethodOrMessageNode ifNil: [ ^ self ].
	self performRefactoringFor: #privateRenameMessageFor: with: aMethodOrMessageNode