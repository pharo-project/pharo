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
	^{OCMessageNode. OCMethodNode}
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
RFSelectorReification >> genForASTMethodNode [
	^entity selector asLiteralNode
]
