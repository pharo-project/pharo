"
I reify the offset of instance variables
"
Class {
	#name : #RFIndexReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFIndexReification class >> entities [
	^{RBVariableNode. RBAssignmentNode}.
]

{ #category : #'plugin interface' }
RFIndexReification class >> key [
	^#index
]

{ #category : #generate }
RFIndexReification >> genForRBAssignmentNode [
	^entity variable binding index asLiteralNode
]

{ #category : #generate }
RFIndexReification >> genForRBVariableNode [
	^entity binding index asLiteralNode
]
