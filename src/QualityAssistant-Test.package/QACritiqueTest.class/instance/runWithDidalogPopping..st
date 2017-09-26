running
runWithDidalogPopping: anAssertingBlock

	UIManager default class = MorphicUIManager
		ifTrue: [ self runWithMorphicDidalogPopping: anAssertingBlock ]
		ifFalse: anAssertingBlock