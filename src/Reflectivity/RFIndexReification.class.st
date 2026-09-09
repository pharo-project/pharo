"
I reify the offset of instance variables
"
Class {
	#name : 'RFIndexReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFIndexReification class >> entities [
	^{OCVariableNode. OCAssignmentNode}
]

{ #category : 'plugin interface' }
RFIndexReification class >> key [
	^#index
]

{ #category : 'generate' }
RFIndexReification >> genForASTAssignmentNode [
	^entity variable binding index asLiteralNode
]

{ #category : 'generate' }
RFIndexReification >> genForASTVariableNode [
	^entity binding index asLiteralNode
]
