"
Using the ObjectRefification, one can pass a pointer to the object where the link is installed in
"
Class {
	#name : 'RFObjectReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFObjectReification class >> entities [
	^{ASTProgramNode. Variable}
]

{ #category : 'plugin interface' }
RFObjectReification class >> key [
	^#object
]

{ #category : 'generate' }
RFObjectReification >> genForASTProgramNode [
	^RBVariableNode selfNode
]

{ #category : 'generate' }
RFObjectReification >> genForInstanceVariableSlot [
	^RBVariableNode selfNode
]

{ #category : 'generate' }
RFObjectReification >> genForLiteralVariable [
	^RBVariableNode selfNode
]
