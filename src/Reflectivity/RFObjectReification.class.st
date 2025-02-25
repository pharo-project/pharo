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
	^{OCProgramNode. Variable}
]

{ #category : 'plugin interface' }
RFObjectReification class >> key [
	^#object
]

{ #category : 'generate' }
RFObjectReification >> genForASTProgramNode [
	^OCVariableNode selfNode
]

{ #category : 'generate' }
RFObjectReification >> genForInstanceVariableSlot [
	^OCVariableNode selfNode
]

{ #category : 'generate' }
RFObjectReification >> genForLiteralVariable [
	^OCVariableNode selfNode
]
