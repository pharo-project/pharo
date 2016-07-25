spec protocol
onMorphTreeSelectionChanged: ann
	| items |
	
	items := (ann selection
		ifNil: [ #() ]
		ifNotNil: [:selection | selection selectedNodes ]).
		
	items := items collect: [ :each | each nodeModel ].

	self model selectedItems: items