"
I reify the name of variables
"
Class {
	#name : 'RFNameReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFNameReification class >> entities [
	^{OCVariableNode. OCAssignmentNode. Variable}
]

{ #category : 'plugin interface' }
RFNameReification class >> key [
	^#name
]

{ #category : 'generate' }
RFNameReification >> genForASTAssignmentNode [
	^entity variable name asLiteralNode
]

{ #category : 'generate' }
RFNameReification >> genForASTVariableNode [
	^entity name asLiteralNode
]

{ #category : 'generate' }
RFNameReification >> genForInstanceVariableSlot [
	^entity name asLiteralNode
]

{ #category : 'generate' }
RFNameReification >> genForLiteralVariable [
	^entity name asLiteralNode
]
