refactoring
sourceTextArea
	| window |
	window := model dependents ifEmpty: [ nil ] ifNotEmpty: [ model dependents at: 1  ].
	
	^window submorphThat: [ :each | each respondsTo: #selectionInterval ] ifNone: [ nil ]