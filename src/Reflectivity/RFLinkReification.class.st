"
I reify the link itself. 
"
Class {
	#name : #RFLinkReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFLinkReification class >> entities [
	^{RBProgramNode . Variable}
]

{ #category : #'plugin interface' }
RFLinkReification class >> key [
	^#link
]

{ #category : #generate }
RFLinkReification >> genForInstanceVariableSlot [
	^RBLiteralNode value: link
]

{ #category : #generate }
RFLinkReification >> genForLiteralVariable [
	^RBLiteralNode value: link
]

{ #category : #generate }
RFLinkReification >> genForRBProgramNode [
	^RBLiteralNode value: link
]
