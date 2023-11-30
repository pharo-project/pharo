"
I reify operations like message send, variable read, assignment....
"
Class {
	#name : 'RFOperationReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFOperationReification class >> entities [
	^{ASTVariableNode. ASTMessageNode. ASTMethodNode . ASTReturnNode. ASTAssignmentNode. ASTLiteralNode. ASTBlockNode . ASTLiteralArrayNode. ASTArrayNode}
]

{ #category : 'plugin interface' }
RFOperationReification class >> key [
	^#operation
]

{ #category : 'generate' }
RFOperationReification >> genForRBArrayNode [
	^ASTVariableNode named: #RFReifyValueVar
]

{ #category : 'generate' }
RFOperationReification >> genForRBAssignmentNode [
	entity variable isLocalVariable ifTrue: [
		^ASTParser parseExpression: ('RFTempWrite new
			assignedValue: RFNewValueReificationVar;
			context: thisContext;
			variableName: #{1}.' format: {entity variable name})].

	entity variable isInstanceVariable ifTrue: [
		^ASTParser parseExpression: ('RFSlotWrite new
			assignedValue: RFNewValueReificationVar;
			object: self;
			variableName: #{1}.' format: {entity variable name})].

	entity variable variable isLiteralVariable ifTrue: [ | ast |
		ast := ASTParser parseExpression: ('RFGlobalWrite new
			assignedValue: RFNewValueReificationVar;
			variable: #toReplace.').
		ast messages second arguments: {(ASTLiteralNode value: entity variable binding)}.
		^ast].
	self error: 'not supported'
]

{ #category : 'generate' }
RFOperationReification >> genForRBBlockNode [
	"we just compile the block again"
	| blockNode |

	blockNode := ASTParser parseExpression: entity formattedCode.
	^ASTMessageNode
		receiver: (ASTVariableNode named: #RFBlockOperation)
		selector: #block:
		arguments: {blockNode}
]

{ #category : 'generate' }
RFOperationReification >> genForRBLiteralArrayNode [
	"same as #value, no need to wrap as the literal understands #value"
	^ASTLiteralArrayNode value: entity value
]

{ #category : 'generate' }
RFOperationReification >> genForRBLiteralValueNode [
	"same as #value, no need to wrap as the literal understands #value"
	^ASTLiteralNode value: entity value
]

{ #category : 'generate' }
RFOperationReification >> genForRBMessageNode [
	^ASTParser parseExpression: ('RFMessageOperation
			receiver: RFReceiverReificationVar
			selector: #{1}
			arguments: RFArgumentsReificationVar.' format: {entity selector})
]

{ #category : 'generate' }
RFOperationReification >> genForRBMethodNode [
	| ast |
	ast := ASTParser parseExpression: 'RFMethodOperation new
			receiver: self;
			method: #toReplace;
			arguments: RFArgumentsReificationVar.'.

	ast messages second arguments: {(ASTLiteralNode value: entity methodNode methodClass >> entity methodNode selector)}.
	^ast
]

{ #category : 'generate' }
RFOperationReification >> genForRBReturnNode [
	^ASTParser parseExpression: 'RFReturnOperation new
			context: thisContext;
			returnValue: RFReifyValueVar'
]

{ #category : 'generate' }
RFOperationReification >> genForRBVariableNode [
	entity isInstanceVariable ifTrue: [
		^ASTParser parseExpression: ('RFSlotRead new
			object: self;
			variableName: #{1}.' format: {entity name})].

	entity isLocalVariable ifTrue: [
		^ASTParser parseExpression: ('RFTempRead new
			context: thisContext;
			variableName: #{1}.' format: {entity name})].

	entity variable isLiteralVariable ifTrue: [ | ast |
			ast := ASTParser parseExpression: 'RFGlobalRead new
				variable: #toReplace'.
			ast arguments: {(ASTLiteralNode value: entity binding)}.

			^ast.
		].

	self error: 'not supported'
]

{ #category : 'preamble' }
RFOperationReification >> preambleForArray: aNode [
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
	preamble add: (RFStorePopIntoTempNode named: 'RFReifyValueVar').
	preamble addAll: arguments.
	^ preamble
]

{ #category : 'preamble' }
RFOperationReification >> preambleForAssignment: aNode [
	^RFStoreIntoTempNode named: #RFNewValueReificationVar
]

{ #category : 'preamble' }
RFOperationReification >> preambleForMessage: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := OrderedCollection new.

	(1 to: aNode numArgs) reverseWithIndexDo: [:each :index |
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			preamble add:  (RFStorePopIntoTempNode named: name).
	].

	preamble
		add: (RFStoreIntoTempNode named: #RFReceiverReificationVar).

	(1 to: aNode numArgs) withIndexDo: [:each :index |
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			arguments add:  (ASTVariableNode named: name).
	].
	preamble addAll: (ASTArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	preamble addAll: arguments.
	^ preamble
]

{ #category : 'preamble' }
RFOperationReification >> preambleForMethod: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := aNode argumentNames collect: [ :name | ASTVariableNode named: name].
	preamble addAll: (ASTArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	^ preamble
]

{ #category : 'preamble' }
RFOperationReification >> preambleForReturn: aNode [
	^RFStoreIntoTempNode named: #RFReifyValueVar
]
