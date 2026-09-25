Extension { #name : 'CmdCompositeToolContext' }

{ #category : '*Commander-Activators-DragAndDrop' }
CmdCompositeToolContext >> allowsDropExecutionOf: aCommand [

	^ self existingContexts anySatisfy: [ :each | each allowsDropExecutionOf: aCommand ]
]

{ #category : '*Commander-Activators-DragAndDrop' }
CmdCompositeToolContext >> applyDropResultOf: aCommand [

	| activeDropContext |
	activeDropContext := self existingContexts detect: [ :each | each allowsDropExecutionOf: aCommand ].

	activeDropContext applyDropResultOf: aCommand
]

{ #category : '*Commander-Activators-DragAndDrop' }
CmdCompositeToolContext >> prepareDragActivationOf: aCommand [

	(self activeContextFor: aCommand)
		prepareDragActivationOf: aCommand
]

{ #category : '*Commander-Activators-DragAndDrop' }
CmdCompositeToolContext >> prepareDropExecutionOf: aCommand [

	| activeDropContext |
	activeDropContext := self existingContexts detect: [ :each | each allowsDropExecutionOf: aCommand ].

	activeDropContext prepareDropExecutionOf: aCommand
]
