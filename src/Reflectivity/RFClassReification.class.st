"
request reification of the class
"
Class {
	#name : 'RFClassReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFClassReification class >> entities [
	^{ASTProgramNode. Variable}
]

{ #category : 'plugin interface' }
RFClassReification class >> key [
	^#class
]

{ #category : 'generate' }
RFClassReification >> genForASTProgramNode [
	^self generate
]

{ #category : 'generate' }
RFClassReification >> genForInstanceVariableSlot [
	^self generate
]

{ #category : 'generate' }
RFClassReification >> genForLiteralVariable [
	^self generate
]

{ #category : 'generate' }
RFClassReification >> generate [
	^RBMessageNode
		receiver: RBVariableNode selfNode
		selector: #class
]
