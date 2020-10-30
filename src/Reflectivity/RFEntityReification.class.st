"
#entity stands for the structural entity that the link is installed on. It is #node for AST nodes, #variable for variables.
"
Class {
	#name : #RFEntityReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFEntityReification class >> entities [
	^{RBProgramNode. Variable}.
]

{ #category : #'plugin interface' }
RFEntityReification class >> key [
	^#entity
]

{ #category : #generate }
RFEntityReification >> genForInstanceVariableSlot [
	
	^RBLiteralNode value: entity
]

{ #category : #generate }
RFEntityReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal"
	^RBLiteralNode value: entity
]

{ #category : #generate }
RFEntityReification >> genForRBProgramNode [
	
	^RBLiteralNode value: entity
]
