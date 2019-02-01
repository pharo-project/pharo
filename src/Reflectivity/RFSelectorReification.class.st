"
I am the selector of a message send or method
"
Class {
	#name : #RFSelectorReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFSelectorReification class >> entities [
	^{RBMessageNode. RBMethodNode}
]

{ #category : #'plugin interface' }
RFSelectorReification class >> key [
	^#selector
]

{ #category : #generate }
RFSelectorReification >> genForRBMessageNode [
	^entity selector asLiteralNode
]

{ #category : #generate }
RFSelectorReification >> genForRBMethodNode [
	^entity selector asLiteralNode
]
