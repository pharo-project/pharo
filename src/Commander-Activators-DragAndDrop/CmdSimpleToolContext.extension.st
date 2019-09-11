Extension { #name : #CmdSimpleToolContext }

{ #category : #'*Commander-Activators-DragAndDrop' }
CmdSimpleToolContext >> allowsDropExecutionOf: aCommand [
	^aCommand canBeExecutedInDropContext: tool
]

{ #category : #'*Commander-Activators-DragAndDrop' }
CmdSimpleToolContext >> applyDropResultOf: aCommand [
	aCommand applyDropResultInContext: tool
]

{ #category : #'*Commander-Activators-DragAndDrop' }
CmdSimpleToolContext >> prepareDragActivationOf: aCommand [
	aCommand prepareExecutionInDragContext: tool
]

{ #category : #'*Commander-Activators-DragAndDrop' }
CmdSimpleToolContext >> prepareDropExecutionOf: aCommand [
	aCommand prepareExecutionInDropContext: tool
]
