"
I reify #variable

- GlobaVar (or subclass) 
- Slot for ivars 
"
Class {
	#name : #RFVariableReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFVariableReification class >> entities [
	^{RBVariableNode. LiteralVariable . Slot}
]

{ #category : #'plugin interface' }
RFVariableReification class >> key [
	^#variable
]

{ #category : #generate }
RFVariableReification >> genForInstanceVariableSlot [
	"entity in this case is the binding of the literal variable"
	^entity asLiteralNode
]

{ #category : #generate }
RFVariableReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal variable"
	^entity asLiteralNode
]

{ #category : #generate }
RFVariableReification >> genForRBVariableNode [
	
	entity isTemp ifTrue: [ ^self error: 'Temps can not be reified yet' ]. 
	^entity binding variable asLiteralNode
]
