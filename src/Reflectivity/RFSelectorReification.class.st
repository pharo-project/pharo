"
I am the selector of a message send or method
"
Class {
	#name : 'RFSelectorReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFSelectorReification class >> entities [
	^{ASTMessageNode. RBMethodNode}
]

{ #category : 'plugin interface' }
RFSelectorReification class >> key [
	^#selector
]

{ #category : 'generate' }
RFSelectorReification >> genForASTMessageNode [
	^entity selector asLiteralNode
]

{ #category : 'generate' }
RFSelectorReification >> genForRBMethodNode [
	^entity selector asLiteralNode
]
