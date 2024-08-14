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
	^{ASTValueNode . ASTReturnNode. RBMethodNode . Variable}
]

{ #category : 'plugin interface' }
RFValueReification class >> key [
	^#value
]

{ #category : 'generate' }
RFValueReification >> genForASTArrayNode [
	^ASTVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTAssignmentNode [
	"#value for Assignemnt is the old value, is that what we want?"
	^ASTVariableNode named: entity variable name
]

{ #category : 'generate' }
RFValueReification >> genForASTCascadeNode [
	^ASTVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTLiteralArrayNode [
	^ASTLiteralArrayNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> genForASTLiteralValueNode [
	^ASTLiteralValueNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> genForASTReturnNode [
	^ASTVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTVariableNode [
	entity variable
		ifNotNil: [ entity variable isLiteralVariable
				ifTrue: [ ^ ASTLiteralNode value: entity binding value ] ].
	^ ASTVariableNode named: entity name
]

{ #category : 'generate' }
RFValueReification >> genForInstanceVariableSlot [
	"entity in this case is the binding of the instance variable"

	^ RBMessageNode
		receiver: (ASTLiteralNode value: entity)
		selector: #read:
		arguments: {ASTVariableNode selfNode}
]

{ #category : 'generate' }
RFValueReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal"
	^ASTLiteralNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> genForRBBlockNode [
	^ASTVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForRBMessageNode [
	^ASTVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForRBMethodNode [
	^ASTVariableNode named: self varName
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
			arguments add:  (ASTVariableNode named: name).
	].
	preamble addAll: (ASTArrayNode statements: arguments).
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
