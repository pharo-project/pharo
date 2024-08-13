"
I reify the link itself. 
"
Class {
	#name : 'RFLinkReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFLinkReification class >> entities [
	^{ASTProgramNode . Variable}
]

{ #category : 'plugin interface' }
RFLinkReification class >> key [
	^#link
]

{ #category : 'generate' }
RFLinkReification >> genForASTProgramNode [
	^ASTLiteralNode value: link
]

{ #category : 'generate' }
RFLinkReification >> genForInstanceVariableSlot [
	^ASTLiteralNode value: link
]

{ #category : 'generate' }
RFLinkReification >> genForLiteralVariable [
	^ASTLiteralNode value: link
]
