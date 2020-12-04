"
I reify the name of variables
"
Class {
	#name : #RFNameReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFNameReification class >> entities [
	^{RBVariableNode. RBAssignmentNode. Variable}.
]

{ #category : #'plugin interface' }
RFNameReification class >> key [
	^#name
]

{ #category : #generate }
RFNameReification >> genForInstanceVariableSlot [
	^entity name asLiteralNode
]

{ #category : #generate }
RFNameReification >> genForLiteralVariable [
	^entity name asLiteralNode
]

{ #category : #generate }
RFNameReification >> genForRBAssignmentNode [
	^entity variable name asLiteralNode
]

{ #category : #generate }
RFNameReification >> genForRBVariableNode [
	^entity name asLiteralNode
]
