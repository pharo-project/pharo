"
I reify operations like message send, variable read, assignment....
"
Class {
	#name : #RFOperationReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFOperationReification class >> entities [
	^{RBVariableNode. RBMessageNode. RBMethodNode}
]

{ #category : #'plugin interface' }
RFOperationReification class >> key [
	^#operation
]

{ #category : #generate }
RFOperationReification >> genForRBMessageNode [
	^RBParser parseExpression: ('RFMessageOperation 
			receiver: RFReceiverReificationVar 
			selector: #{1} 
			arguments: RFArgumentsReificationVar.' format: {entity selector})

]

{ #category : #generate }
RFOperationReification >> genForRBMethodNode [
	| ast |
	ast := RBParser parseExpression: 'RFMethodOperation new
			receiver: self;
			method: #toReplace;
			arguments: RFArgumentsReificationVar.'.	
	
	ast messages second arguments: {(RFLiteralVariableNode value: entity methodNode method)}.
	^ast.
]

{ #category : #generate }
RFOperationReification >> genForRBVariableNode [
	entity isInstance ifTrue: [  
		^RBParser parseExpression: ('RFSlotRead new 
			object: self;
			variableName: #{1}.' format: {entity name})].
		
	entity isTemp ifTrue: [
		^RBParser parseExpression: ('RFTempRead new 
			context: thisContext;
			variableName: #{1}.' format: {entity name})].
	
	entity isGlobal ifTrue: [
			^RBParser parseExpression: ('RFGlobalRead new 
			variableName: #{1}.' format: {entity name})].
		
	self error: 'not supported'


]

{ #category : #generate }
RFOperationReification >> preamble: aNode [
	aNode isMessage ifTrue: [ ^self preambleForMessage: aNode ].
	aNode isMethod  ifTrue: [ ^self preambleForMethod: aNode ].
	^super preamble: aNode.
]

{ #category : #generate }
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
			arguments add:  (RBTemporaryNode named: name).
	].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	preamble addAll: arguments.
	^ preamble 
]

{ #category : #generate }
RFOperationReification >> preambleForMethod: aNode [
	| preamble arguments |
	preamble := OrderedCollection new.
	arguments := aNode argumentNames collect: [ :name | RBTemporaryNode named: name].
	preamble addAll: (RBArrayNode statements: arguments).
	preamble add: (RFStorePopIntoTempNode named: 'RFArgumentsReificationVar').
	^ preamble 
]
