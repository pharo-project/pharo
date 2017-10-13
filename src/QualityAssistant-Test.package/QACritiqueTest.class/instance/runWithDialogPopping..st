running
runWithDialogPopping: anAssertingBlock
 
	UIManager default class = MorphicUIManager
		ifTrue: [ self runWithMorphicDialogPopping: anAssertingBlock ]
		ifFalse: anAssertingBlock
