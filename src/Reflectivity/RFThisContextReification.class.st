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
	^RBThisContextNode new
]

{ #category : #generate }
RFThisContextReification >> genForLiteralVariable [
	^RBThisContextNode new
]

{ #category : #generate }
RFThisContextReification >> genForRBProgramNode [
	^RBThisContextNode new
]
