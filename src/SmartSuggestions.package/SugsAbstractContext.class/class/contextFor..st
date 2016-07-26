instance creation
contextFor: anEditor
	(anEditor respondsTo: #sugsContext)
		ifTrue: [ ^ anEditor sugsContext ].
	(anEditor model respondsTo: #interactionModel)
		ifTrue: [ 
			(anEditor model interactionModel respondsTo: #sugsContext)
				ifTrue: [ ^ anEditor model interactionModel sugsContext ] ].
	
	^ SugsNullContext new