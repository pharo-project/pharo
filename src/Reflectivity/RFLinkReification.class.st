"
I reify the link itself. 
"
Class {
	#name : #RFLinkReification,
	#superclass : #RFReification,
	#category : #Reflectivity-Links
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
	^link asLiteralNode
]

{ #category : #generate }
RFLinkReification >> genForLiteralVariable [
	^link asLiteralNode
]

{ #category : #generate }
RFLinkReification >> genForRBProgramNode [
	^link asLiteralNode
]
