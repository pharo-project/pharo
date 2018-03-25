"
#entity stands for the structural entity that the link is installed on. It is #node for AST nodes, #variable for variables.
"
Class {
	#name : #RFEntityReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFEntityReification class >> entities [
	^{RBProgramNode. LiteralVariable. Slot}.
]

{ #category : #'plugin interface' }
RFEntityReification class >> key [
	^#entity
]

{ #category : #generate }
RFEntityReification >> genForInstanceVariableSlot [
	
	^RFLiteralVariableNode value: entity
]

{ #category : #generate }
RFEntityReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal variable"
	^RFLiteralVariableNode value: entity
]

{ #category : #generate }
RFEntityReification >> genForRBProgramNode [
	
	^RFLiteralVariableNode value: entity
]
