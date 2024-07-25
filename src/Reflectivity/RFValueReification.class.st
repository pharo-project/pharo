"
value

Assignment:  the orginal value for before, new value for after. 

Variables: current value (same for before and after)

Message: result of the message, only availabe for *after* links.

Return: what will be returned


"
Class {
	#name : 'RFValueReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFValueReification class >> entities [
	^{ASTValueNode . RBReturnNode. RBMethodNode . Variable}
]

{ #category : 'plugin interface' }
RFValueReification class >> key [
	^#value
]

{ #category : 'generate' }
RFValueReification >> genForInstanceVariableSlot [
	"entity in this case is the binding of the instance variable"

	^ RBMessageNode
		receiver: (ASTLiteralNode value: entity)
		selector: #read:
		arguments: {RBVariableNode selfNode}
]

{ #category : 'generate' }
RFValueReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal"
	^ASTLiteralNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> genForRBArrayNode [
	^RBVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForRBAssignmentNode [
	"#value for Assignemnt is the old value, is that what we want?"
	^RBVariableNode named: entity variable name
]

{ #category : 'generate' }
RFValueReification >> genForRBBlockNode [
	^RBVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForRBCascadeNode [
	^RBVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForRBLiteralArrayNode [
	^RBLiteralArrayNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> genForRBLiteralValueNode [
	^RBLiteralValueNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> genForRBMessageNode [
	^RBVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForRBMethodNode [
	^RBVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForRBReturnNode [
	^RBVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForRBVariableNode [
	entity variable
		ifNotNil: [ entity variable isLiteralVariable
				ifTrue: [ ^ ASTLiteralNode value: entity binding value ] ].
	^ RBVariableNode named: entity name
]

{ #category : 'generate' }
RFValueReification >> postamble: aNode [
	(aNode isKindOf: ASTProgramNode) ifFalse: [ ^#() ].
	aNode isMessage ifTrue: [^RFStoreIntoTempNode named: self varName. ].
	aNode isCascade ifTrue: [^RFStoreIntoTempNode named: self varName. ].
	aNode isBlock ifTrue: [^RFStoreIntoTempNode named: self varName].
	^super postamble: aNode
]

{ #category : 'preamble' }
RFValueReification >> preambleForArray: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := OrderedCollection new.

	(1 to: aNode size) reverseWithIndexDo: [:each :index |
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			preamble add:  (RFStorePopIntoTempNode named: name).
	].

	self flag: #pharoTodo. "for #instead we do not need to build the stack up again as the array creation bytecode is missing"
	(1 to: aNode size) withIndexDo: [:each :index |
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			arguments add:  (RBVariableNode named: name).
	].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: self varName).
	preamble addAll: arguments.
	^ preamble
]

{ #category : 'preamble' }
RFValueReification >> preambleForReturn: aNode [
	^ RFStoreIntoTempNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> varName [
	^#RFReifyValueVar
]
