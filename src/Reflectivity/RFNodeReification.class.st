"
I am the node that the link is installed on
"
Class {
	#name : #RFNodeReification,
	#superclass : #RFReification,
	#category : #Reflectivity-Links
}

{ #category : #'plugin interface' }
RFNodeReification class >> entities [
	^{RBProgramNode}.
]

{ #category : #'plugin interface' }
RFNodeReification class >> key [
	^#node
]

{ #category : #generate }
RFNodeReification >> genForRBProgramNode [
	^RFLiteralVariableNode value: entity
]
