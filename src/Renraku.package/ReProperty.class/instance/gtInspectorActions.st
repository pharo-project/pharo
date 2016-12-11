actions
gtInspectorActions
	^ (self actions collect: [ :act | 
		GLMGenericAction new
			action: [ act actOnCritic: self ofEntity: self sourceAnchor entity ];
		icon: act icon;
		title: act description;
		yourself ]),
	super gtInspectorActions