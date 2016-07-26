refactoring
sourceTextArea
	^ model dependents ifEmpty: [ nil ] ifNotEmpty: [ model dependents at: 1  ]