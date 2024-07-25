"
I can be used to pass the context to the meta object
"
Class {
	#name : 'RFThisContextReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFThisContextReification class >> entities [
	^{ASTProgramNode . Variable}
]

{ #category : 'plugin interface' }
RFThisContextReification class >> key [
	^#context
]

{ #category : 'generate' }
RFThisContextReification >> genForASTProgramNode [
	^RBVariableNode thisContextNode
]

{ #category : 'generate' }
RFThisContextReification >> genForInstanceVariableSlot [
	^RBVariableNode thisContextNode
]

{ #category : 'generate' }
RFThisContextReification >> genForLiteralVariable [
	^RBVariableNode thisContextNode
]
