"
I reify the link itself. 
"
Class {
	#name : #RFLinkReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFLinkReification class >> entities [
	^{RBProgramNode . LiteralVariable . Slot}
]

{ #category : #'plugin interface' }
RFLinkReification class >> key [
	^#link
]

{ #category : #generate }
RFLinkReification >> genForInstanceVariableSlot [
	^RFLiteralVariableNode value: link
]

{ #category : #generate }
RFLinkReification >> genForLiteralVariable [
	^RFLiteralVariableNode value: link
]

{ #category : #generate }
RFLinkReification >> genForRBProgramNode [
	^RFLiteralVariableNode value: link
]
