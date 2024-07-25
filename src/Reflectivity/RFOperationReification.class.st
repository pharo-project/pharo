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
	^{RBVariableNode. RBMessageNode. RBMethodNode . RBReturnNode. RBAssignmentNode. ASTLiteralNode. RBBlockNode . RBLiteralArrayNode. RBArrayNode}
]

{ #category : 'plugin interface' }
RFOperationReification class >> key [
	^#operation
]

{ #category : 'generate' }
RFOperationReification >> genForRBArrayNode [
	^RBVariableNode named: #RFReifyValueVar
]

{ #category : 'generate' }
RFOperationReification >> genForRBAssignmentNode [
	entity variable isLocalVariable ifTrue: [
		^RBParser parseExpression: ('RFTempWrite new
			assignedValue: RFNewValueReificationVar;
			context: thisContext;
			variableName: #{1}.' format: {entity variable name})].

	entity variable isInstanceVariable ifTrue: [
		^RBParser parseExpression: ('RFSlotWrite new
			assignedValue: RFNewValueReificationVar;
			object: self;
			variableName: #{1}.' format: {entity variable name})].

	entity variable variable isLiteralVariable ifTrue: [ | ast |
		ast := RBParser parseExpression: ('RFGlobalWrite new
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

	blockNode := RBParser parseExpression: entity formattedCode.
	^RBMessageNode
		receiver: (RBVariableNode named: #RFBlockOperation)
		selector: #block:
		arguments: {blockNode}
]

{ #category : 'generate' }
RFOperationReification >> genForRBLiteralArrayNode [
	"same as #value, no need to wrap as the literal understands #value"
	^RBLiteralArrayNode value: entity value
]

{ #category : 'generate' }
RFOperationReification >> genForRBLiteralValueNode [
	"same as #value, no need to wrap as the literal understands #value"
	^ASTLiteralNode value: entity value
]

{ #category : 'generate' }
RFOperationReification >> genForRBMessageNode [
	^RBParser parseExpression: ('RFMessageOperation
			receiver: RFReceiverReificationVar
			selector: #{1}
			arguments: RFArgumentsReificationVar.' format: {entity selector})
]

{ #category : 'generate' }
RFOperationReification >> genForRBMethodNode [
	| ast |
	ast := RBParser parseExpression: 'RFMethodOperation new
			receiver: self;
			method: #toReplace;
			arguments: RFArgumentsReificationVar.'.

	ast messages second arguments: {(ASTLiteralNode value: entity methodNode methodClass >> entity methodNode selector)}.
	^ast
]

{ #category : 'generate' }
RFOperationReification >> genForRBReturnNode [
	^RBParser parseExpression: 'RFReturnOperation new
			context: thisContext;
			returnValue: RFReifyValueVar'
]

{ #category : 'generate' }
RFOperationReification >> genForRBVariableNode [
	entity isInstanceVariable ifTrue: [
		^RBParser parseExpression: ('RFSlotRead new
			object: self;
			variableName: #{1}.' format: {entity name})].

	entity isLocalVariable ifTrue: [
		^RBParser parseExpression: ('RFTempRead new
			context: thisContext;
			variableName: #{1}.' format: {entity name})].

	entity variable isLiteralVariable ifTrue: [ | ast |
			ast := RBParser parseExpression: 'RFGlobalRead new
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
			arguments add:  (RBVariableNode named: name).
	].
	preamble addAll: (RBArrayNode statements: arguments).
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
			arguments add:  (RBVariableNode named: name).
	].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	preamble addAll: arguments.
	^ preamble
]

{ #category : 'preamble' }
RFOperationReification >> preambleForMethod: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := aNode argumentNames collect: [ :name | RBVariableNode named: name].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	^ preamble
]

{ #category : 'preamble' }
RFOperationReification >> preambleForReturn: aNode [
	^RFStoreIntoTempNode named: #RFReifyValueVar
]
