Extension { #name : 'CmdSimpleToolContext' }

{ #category : '*Commander-Activators-DragAndDrop' }
CmdSimpleToolContext >> allowsDropExecutionOf: aCommand [

	^ aCommand canBeExecutedInDropContext: self tool
]

{ #category : '*Commander-Activators-DragAndDrop' }
CmdSimpleToolContext >> applyDropResultOf: aCommand [

	aCommand applyDropResultInContext: self tool
]

{ #category : '*Commander-Activators-DragAndDrop' }
CmdSimpleToolContext >> prepareDragActivationOf: aCommand [

	aCommand prepareExecutionInDragContext: self tool
]

{ #category : '*Commander-Activators-DragAndDrop' }
CmdSimpleToolContext >> prepareDropExecutionOf: aCommand [

	aCommand prepareExecutionInDropContext: self tool
]
