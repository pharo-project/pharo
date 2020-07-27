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
	^{RBProgramNode . Variable}
]

{ #category : #'plugin interface' }
RFThisContextReification class >> key [
	^#context
]

{ #category : #generate }
RFThisContextReification >> genForInstanceVariableSlot [
	^RBVariableNode named: 'thisContext'
]

{ #category : #generate }
RFThisContextReification >> genForLiteralVariable [
	^RBVariableNode named: 'thisContext'
]

{ #category : #generate }
RFThisContextReification >> genForRBProgramNode [
	^RBVariableNode named: 'thisContext'
]
