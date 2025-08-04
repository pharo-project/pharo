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
	^{OCProgramNode . Variable}
]

{ #category : 'plugin interface' }
RFLinkReification class >> key [
	^#link
]

{ #category : 'generate' }
RFLinkReification >> genForASTProgramNode [
	^OCLiteralNode value: link
]

{ #category : 'generate' }
RFLinkReification >> genForInstanceVariableSlot [
	^OCLiteralNode value: link
]

{ #category : 'generate' }
RFLinkReification >> genForLiteralVariable [
	^OCLiteralNode value: link
]
