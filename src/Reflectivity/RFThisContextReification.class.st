"
I can be used to pass the context to the meta object
"
Class {
	#name : #RFThisContextReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFThisContextReification class >> entities [
	^{RBProgramNode . LiteralVariable . Slot}
]

{ #category : #'plugin interface' }
RFThisContextReification class >> key [
	^#context
]

{ #category : #generate }
RFThisContextReification >> genForInstanceVariableSlot [
	^RBVariableNode newThisContextNode 
]

{ #category : #generate }
RFThisContextReification >> genForLiteralVariable [
	^RBVariableNode newThisContextNode 
]

{ #category : #generate }
RFThisContextReification >> genForRBProgramNode [
	^ RBVariableNode newThisContextNode
]
