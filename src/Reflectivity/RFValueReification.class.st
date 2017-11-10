"
value of a variable read or assignment
"
Class {
	#name : #RFValueReification,
	#superclass : #RFReification,
	#category : #Reflectivity-Links
}

{ #category : #'plugin interface' }
RFValueReification class >> entities [
	^{RBVariableNode. RBAssignmentNode. RBReturnNode. RBMessageNode . LiteralVariable . Slot}
]

{ #category : #'plugin interface' }
RFValueReification class >> key [
	^#value
]

{ #category : #generate }
RFValueReification >> genForInstanceVariableSlot [
	"entity in this case is the binding of the instance variable"
	^RBMessageNode receiver: entity asLiteralNode selector: #read: arguments: { RBVariableNode named: 'self' }
]

{ #category : #generate }
RFValueReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal variable"
	^entity value asLiteralNode
]

{ #category : #generate }
RFValueReification >> genForRBAssignmentNode [
	^RBVariableNode named: entity variable name
]

{ #category : #generate }
RFValueReification >> genForRBMessageNode [
	^RBVariableNode named: #RFReifyValueVar
]

{ #category : #generate }
RFValueReification >> genForRBReturnNode [
	^RBVariableNode named: #RFReifyValueVar
]

{ #category : #generate }
RFValueReification >> genForRBVariableNode [
	entity isGlobal ifTrue: [^entity binding value asLiteralNode]. 
	^RBVariableNode named: entity name.
]

{ #category : #generate }
RFValueReification >> preamble: aNode [
	(aNode isKindOf: RBProgramNode) ifFalse: [ ^#() ].
	^(aNode isReturn or: [ aNode isMessage])
		ifTrue: [RFStoreIntoTempNode named: #RFReifyValueVar]
		ifFalse: [ #() ]. 
]
