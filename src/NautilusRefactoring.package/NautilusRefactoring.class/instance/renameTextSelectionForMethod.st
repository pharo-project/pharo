source
renameTextSelectionForMethod
	| selectedInterval  method tempNode |
	method := self model selectedMethod.
	selectedInterval := self model selectionInterval.
	selectedInterval first = 1
		ifTrue: [ ^ self renameMethodFor: method ].
	tempNode := (method ast bestNodeFor: selectedInterval).
	tempNode executeRenameRefactoringBy: self