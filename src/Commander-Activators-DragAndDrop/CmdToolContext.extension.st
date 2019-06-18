Extension { #name : #CmdToolContext }

{ #category : #'*Commander-Activators-DragAndDrop' }
CmdToolContext >> allowsDropExecutionOf: aCommand [
	^aCommand canBeExecutedInDropContext: self
]

{ #category : #'*Commander-Activators-DragAndDrop' }
CmdToolContext >> applyDropResultOf: aCommand [
	aCommand applyDropResultInContext: self
]

{ #category : #'*Commander-Activators-DragAndDrop' }
CmdToolContext >> prepareDragActivationOf: aCommand [
	aCommand prepareExecutionInDragContext: self
]

{ #category : #'*Commander-Activators-DragAndDrop' }
CmdToolContext >> prepareDropExecutionOf: aCommand [
	aCommand prepareExecutionInDropContext: self
]
