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
	^{OCVariableNode. OCMessageNode. OCMethodNode . OCReturnNode. OCAssignmentNode. OCLiteralNode. OCBlockNode . OCLiteralArrayNode. OCArrayNode}
]

{ #category : 'plugin interface' }
RFOperationReification class >> key [
	^#operation
]

{ #category : 'generate' }
RFOperationReification >> genForASTArrayNode [
	^OCVariableNode named: #RFReifyValueVar
]

{ #category : 'generate' }
RFOperationReification >> genForASTAssignmentNode [
	entity variable isLocalVariable ifTrue: [
		^OCParser parseExpression: ('RFTempWrite new
			assignedValue: RFNewValueReificationVar;
			context: thisContext;
			variableName: #{1}.' format: {entity variable name})].

	entity variable isInstanceVariable ifTrue: [
		^OCParser parseExpression: ('RFSlotWrite new
			assignedValue: RFNewValueReificationVar;
			object: self;
			variableName: #{1}.' format: {entity variable name})].

	entity variable variable isLiteralVariable ifTrue: [ | ast |
		ast := OCParser parseExpression: ('RFGlobalWrite new
			assignedValue: RFNewValueReificationVar;
			variable: #toReplace.').
		ast messages second arguments: {(OCLiteralNode value: entity variable binding)}.
		^ast].
	self error: 'not supported'
]

{ #category : 'generate' }
RFOperationReification >> genForASTBlockNode [
	"we just compile the block again"
	| blockNode |

	blockNode := OCParser parseExpression: entity formattedCode.
	^OCMessageNode
		receiver: (OCVariableNode named: #RFBlockOperation)
		selector: #block:
		arguments: {blockNode}
]

{ #category : 'generate' }
RFOperationReification >> genForASTLiteralArrayNode [
	"same as #value, no need to wrap as the literal understands #value"
	^OCLiteralArrayNode value: entity value
]

{ #category : 'generate' }
RFOperationReification >> genForASTLiteralValueNode [
	"same as #value, no need to wrap as the literal understands #value"
	^OCLiteralNode value: entity value
]

{ #category : 'generate' }
RFOperationReification >> genForASTMessageNode [
	^OCParser parseExpression: ('RFMessageOperation
			receiver: RFReceiverReificationVar
			selector: #{1}
			arguments: RFArgumentsReificationVar.' format: {entity selector})
]

{ #category : 'generate' }
RFOperationReification >> genForASTMethodNode [
	| ast |
	ast := OCParser parseExpression: 'RFMethodOperation new
			receiver: self;
			method: #toReplace;
			arguments: RFArgumentsReificationVar.'.

	ast messages second arguments: {(OCLiteralNode value: entity methodNode methodClass >> entity methodNode selector)}.
	^ast
]

{ #category : 'generate' }
RFOperationReification >> genForASTReturnNode [
	^OCParser parseExpression: 'RFReturnOperation new
			context: thisContext;
			returnValue: RFReifyValueVar'
]

{ #category : 'generate' }
RFOperationReification >> genForASTVariableNode [
	entity isInstanceVariable ifTrue: [
		^OCParser parseExpression: ('RFSlotRead new
			object: self;
			variableName: #{1}.' format: {entity name})].

	entity isLocalVariable ifTrue: [
		^OCParser parseExpression: ('RFTempRead new
			context: thisContext;
			variableName: #{1}.' format: {entity name})].

	entity variable isLiteralVariable ifTrue: [ | ast |
			ast := OCParser parseExpression: 'RFGlobalRead new
				variable: #toReplace'.
			ast arguments: {(OCLiteralNode value: entity binding)}.

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
			arguments add:  (OCVariableNode named: name).
	].
	preamble addAll: (OCArrayNode statements: arguments).
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
			arguments add:  (OCVariableNode named: name).
	].
	preamble addAll: (OCArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	preamble addAll: arguments.
	^ preamble
]

{ #category : 'preamble' }
RFOperationReification >> preambleForMethod: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := aNode argumentNames collect: [ :name | OCVariableNode named: name].
	preamble addAll: (OCArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	^ preamble
]

{ #category : 'preamble' }
RFOperationReification >> preambleForReturn: aNode [
	^RFStoreIntoTempNode named: #RFReifyValueVar
]
