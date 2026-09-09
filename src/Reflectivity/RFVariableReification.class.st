"
I reify #variable

- GlobaVar (or subclass) 
- Slot for ivars 
"
Class {
	#name : 'RFVariableReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFVariableReification class >> entities [
	^{OCVariableNode. Variable}
]

{ #category : 'plugin interface' }
RFVariableReification class >> key [
	^#variable
]

{ #category : 'generate' }
RFVariableReification >> genForASTVariableNode [

	entity isTempVariable ifTrue: [ ^self error: 'Temps can not be reified yet' ].
	^OCLiteralNode value: entity binding
]

{ #category : 'generate' }
RFVariableReification >> genForInstanceVariableSlot [
	"entity in this case is the binding of the literal"
	^OCLiteralNode value: entity
]

{ #category : 'generate' }
RFVariableReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal"
	^OCLiteralNode value: entity
]
