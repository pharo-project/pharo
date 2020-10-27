"
I reify #variable

- GlobaVar (or subclass) 
- Slot for ivars 
"
Class {
	#name : #RFVariableReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFVariableReification class >> entities [
	^{RBVariableNode. Variable}
]

{ #category : #'plugin interface' }
RFVariableReification class >> key [
	^#variable
]

{ #category : #generate }
RFVariableReification >> genForInstanceVariableSlot [
	"entity in this case is the binding of the literal"
	^RBLiteralNode value: entity
]

{ #category : #generate }
RFVariableReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal"
	^RBLiteralNode value: entity
]

{ #category : #generate }
RFVariableReification >> genForRBVariableNode [
	
	entity isTempVariable ifTrue: [ ^self error: 'Temps can not be reified yet' ]. 
	^RBLiteralNode value: entity binding
]
