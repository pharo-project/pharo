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
	^{OCValueNode . OCReturnNode. OCMethodNode . Variable}
]

{ #category : 'plugin interface' }
RFValueReification class >> key [
	^#value
]

{ #category : 'generate' }
RFValueReification >> genForASTArrayNode [
	^OCVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTAssignmentNode [
	"#value for Assignemnt is the old value, is that what we want?"
	^OCVariableNode named: entity variable name
]

{ #category : 'generate' }
RFValueReification >> genForASTBlockNode [
	^OCVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTCascadeNode [
	^OCVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTLiteralArrayNode [
	^OCLiteralArrayNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> genForASTLiteralValueNode [
	^OCLiteralValueNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> genForASTMessageNode [
	^OCVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTMethodNode [
	^OCVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTReturnNode [
	^OCVariableNode named: self varName
]

{ #category : 'generate' }
RFValueReification >> genForASTVariableNode [
	entity variable
		ifNotNil: [ entity variable isLiteralVariable
				ifTrue: [ ^ OCLiteralNode value: entity binding value ] ].
	^ OCVariableNode named: entity name
]

{ #category : 'generate' }
RFValueReification >> genForInstanceVariableSlot [
	"entity in this case is the binding of the instance variable"

	^ OCMessageNode
		receiver: (OCLiteralNode value: entity)
		selector: #read:
		arguments: {OCVariableNode selfNode}
]

{ #category : 'generate' }
RFValueReification >> genForLiteralVariable [
	"entity in this case is the binding of the literal"
	^OCLiteralNode value: entity value
]

{ #category : 'generate' }
RFValueReification >> postamble: aNode [
	(aNode isKindOf: OCProgramNode) ifFalse: [ ^#() ].
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
			arguments add:  (OCVariableNode named: name).
	].
	preamble addAll: (OCArrayNode statements: arguments).
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
