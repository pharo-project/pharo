"
I construct the node tree for a Decompiler.
"
Class {
	#name : #DecompilerConstructor,
	#superclass : #ParseNode,
	#instVars : [
		'method',
		'instVars',
		'nArgs',
		'literalValues',
		'tempVars'
	],
	#category : 'Compiler-Support'
}

{ #category : #visiting }
DecompilerConstructor >> accept: aVisitor [
	"I am not really a ParseNode.  Only here to access constants defined in parseNode."
	self shouldNotImplement
]

{ #category : #constructor }
DecompilerConstructor >> codeAnyLitInd: association [

	^VariableNode new
		name: association key
		key: association
		index: 0
		type: LdLitIndType
]

{ #category : #constructor }
DecompilerConstructor >> codeAnyLiteral: value [

	^LiteralNode new
		key: value
		index: 0
		type: LdLitType
]

{ #category : #constructor }
DecompilerConstructor >> codeAnySelector: selector [

	^SelectorNode new
		key: selector
		index: 0
		type: SendType
]

{ #category : #constructor }
DecompilerConstructor >> codeArguments: args block: block [

	^block arguments: args
]

{ #category : #constructor }
DecompilerConstructor >> codeArguments: args temps: temps block: block [ 
	block
		arguments: args;
		temporaries: temps.
	^block
]

{ #category : #constructor }
DecompilerConstructor >> codeAssignTo: variable value: expression [

	^AssignmentNode new variable: variable value: expression
]

{ #category : #constructor }
DecompilerConstructor >> codeBlock: statements returns: returns [
	^ BlockNode statements: statements returns: returns
]

{ #category : #constructor }
DecompilerConstructor >> codeBrace: elements [

	^BraceNode new elements: elements
]

{ #category : #constructor }
DecompilerConstructor >> codeCascade: receiver messages: messages [

	^ (BraceNode new matchBraceStreamReceiver: receiver messages: messages)
		ifNil: [CascadeNode new receiver: receiver messages: messages]
]

{ #category : #constructor }
DecompilerConstructor >> codeCascadedMessage: selector arguments: arguments [

	^self
		codeMessage: nil
		selector: selector
		arguments: arguments
]

{ #category : #constructor }
DecompilerConstructor >> codeConstants [
	"Answer with an array of the objects representing self, true, false, nil,
	-1, 0, 1, 2."

	^(Array with: NodeSelf with: NodeTrue with: NodeFalse with: NodeNil)
		, ((-1 to: 2) collect: [:i | LiteralNode new key: i code: LdMinus1 + i + 1])
]

{ #category : #constructor }
DecompilerConstructor >> codeEmptyBlock [
	^ BlockNode withJust: NodeNil
]

{ #category : #constructor }
DecompilerConstructor >> codeInst: index [

	^InstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index asString])
		index: index + 1
]

{ #category : #constructor }
DecompilerConstructor >> codeMessage: receiver selector: selector arguments: arguments [
	| symbol node |
	symbol := selector key.
	(node := BraceNode new
			matchBraceWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [^ node].
	(node := self decodeIfNilWithReceiver: receiver
			selector: symbol
			arguments: arguments) ifNotNil: [^ node].
	^ MessageNode new
			receiver: receiver selector: selector
			arguments: arguments
			precedence: symbol precedence
]

{ #category : #constructor }
DecompilerConstructor >> codeMethod: selector block: block tempVars: vars primitive: primitive class: class [

	| node methodTemps arguments temporaries |
	node := self codeSelector: selector code: nil.
	tempVars := vars.
	methodTemps := tempVars select: [:t | t scope >= 0].
	arguments := methodTemps copyFrom: 1 to: nArgs.
	temporaries := methodTemps copyFrom: nArgs + 1 to: methodTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	^MethodNode new
		selector: node
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (LegacyEncoder new initScopeAndLiteralTables
					temps: tempVars
					literals: literalValues
					class: class)
		primitive: primitive
]

{ #category : #constructor }
DecompilerConstructor >> codeSelector: sel code: code [

	^SelectorNode new key: sel code: code
]

{ #category : #constructor }
DecompilerConstructor >> codeSuper [

	^NodeSuper
]

{ #category : #constructor }
DecompilerConstructor >> codeTemp: index [

	^ TempVariableNode new
		name: 't' , (index + 1) printString
		index: index
		type: LdTempType
		scope: 0
]

{ #category : #constructor }
DecompilerConstructor >> codeTemp: index named: tempName [

	^ TempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0
]

{ #category : #constructor }
DecompilerConstructor >> codeThisContext [

	^NodeThisContext
]

{ #category : #constructor }
DecompilerConstructor >> decodeIfNilWithReceiver: receiver selector: selector arguments: arguments [
	receiver ifNil: [ ^nil ].		"For instance, when cascading"
	selector == #ifTrue:ifFalse:
		ifFalse: [^ nil].
	(receiver isMessage: #==
				receiver: nil
				arguments: [:argNode | argNode == NodeNil])
		ifFalse: [^ nil].
	^ (MessageNode new
			receiver: receiver
			selector: (SelectorNode new key: #ifTrue:ifFalse: code: #macro)
			arguments: arguments
			precedence: 3)
		noteSpecialSelector: #ifNil:ifNotNil:
]

{ #category : #testing }
DecompilerConstructor >> isForClosures [
	^false
]

{ #category : #initialize-release }
DecompilerConstructor >> method: aMethod class: aClass literals: literals [

	method := aMethod.
	instVars := aClass allInstVarNames.
	nArgs := method numArgs.
	literalValues := literals
]
