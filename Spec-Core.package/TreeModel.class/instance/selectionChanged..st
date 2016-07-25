initialization
selectionChanged: ann

	^ selectionHolder value: (ann selection
		ifNil: [ #() ]
		ifNotNil: [:selection | selection selectedNodes ])