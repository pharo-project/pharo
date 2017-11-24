"
Using the ObjectRefification, one can pass a pointer to the object where the link is installed in
"
Class {
	#name : #RFObjectReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFObjectReification class >> entities [
	^{RBProgramNode. LiteralVariable. Slot}
]

{ #category : #'plugin interface' }
RFObjectReification class >> key [
	^#object
]

{ #category : #generate }
RFObjectReification >> genForInstanceVariableSlot [
	^RBVariableNode named: 'self'
]

{ #category : #generate }
RFObjectReification >> genForLiteralVariable [
	^RBVariableNode named: 'self'
]

{ #category : #generate }
RFObjectReification >> genForRBProgramNode [
	^RBVariableNode named: 'self'
]
