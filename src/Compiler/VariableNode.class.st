"
I am a parse tree leaf representing a variable. Note that my name and key are different for pool variables: the key is the Object Reference.
"
Class {
	#name : #VariableNode,
	#superclass : #LeafNode,
	#instVars : [
		'name'
	],
	#category : #Compiler-ParseNodes
}

{ #category : #initialization }
VariableNode class >> initialize [    "VariableNode initialize.  Decompiler initialize"
	| encoder |
	encoder := LegacyEncoder new.
	StdVariables := Dictionary new: 16.
	encoder
		fillDict: StdVariables
		with: VariableNode
		mapping: #('self' 'thisContext' 'super' 'nil' 'false' 'true' )
		to: (Array with: LdSelf with: LdThisContext with: LdSuper)
				, (Array with: LdNil with: LdFalse with: LdTrue).
	StdSelectors := Dictionary new: 64.
	encoder
		fillDict: StdSelectors
		with: SelectorNode
		mapping: ((1 to: Smalltalk specialSelectorSize) collect: 
							[:i | Smalltalk specialSelectorAt: i])
		to: (SendPlus to: SendPlus + 31).
	StdLiterals := LiteralDictionary new: 16.
	encoder
		fillDict: StdLiterals
		with: LiteralNode
		mapping: #(-1 0 1 2 )
		to: (LdMinus1 to: LdMinus1 + 3).
	encoder initScopeAndLiteralTables.

	NodeNil := encoder encodeVariable: 'nil'.
	NodeTrue := encoder encodeVariable: 'true'.
	NodeFalse := encoder encodeVariable: 'false'.
	NodeSelf := encoder encodeVariable: 'self'.
	NodeThisContext := encoder encodeVariable: 'thisContext'.
	NodeSuper := encoder encodeVariable: 'super'
]

{ #category : #visiting }
VariableNode >> accept: aVisitor [
	^aVisitor visitVariableNode: self
]

{ #category : #initialize-release }
VariableNode >> asStorableNode: encoder [
	^ self
]

{ #category : #testing }
VariableNode >> assignmentCheck: encoder at: location [
	^(encoder cantStoreInto: name) ifTrue: [location] ifFalse: [-1]
]

{ #category : #'code generation (closures)' }
VariableNode >> beingAssignedToAnalyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	"No-op overridden by TempVariableNode"
]

{ #category : #'code generation' }
VariableNode >> emitCodeForReturn: stack encoder: encoder [
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			"short returns"
			 encoder genReturnSpecialLiteral: specialLiteral.
			 stack push: 1 "doesnt seem right".
			 ^self].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue: 
		["short returns"
		 encoder genReturnReceiver.
		 stack push: 1 "doesnt seem right".
		 ^self].
	super emitCodeForReturn: stack encoder: encoder
]

{ #category : #'code generation' }
VariableNode >> emitCodeForStore: stack encoder: encoder [

	self shouldNotImplement
]

{ #category : #'code generation' }
VariableNode >> emitCodeForStorePop: stack encoder: encoder [
	self assert: [self type ~= 1].
	encoder genStorePopInstVar: index.
	stack pop: 1
]

{ #category : #'code generation' }
VariableNode >> emitCodeForValue: stack encoder: encoder [
	stack push: 1.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder genPushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder genPushReceiver].
	code = LdThisContext ifTrue:
		[^encoder genPushThisContext].
	self flag: 'probably superfluous'.
	^encoder genPushInstVar: index
]

{ #category : #'code generation' }
VariableNode >> fieldOffset [
	"Return temp or instVar offset for this variable"
	^index ifNil: [code < 256
					ifTrue: [code \\ 16]
					ifFalse: [code \\ 256]]
]

{ #category : #testing }
VariableNode >> index [
	"This code attempts to reconstruct the index from its encoding in code."
	code < 0 ifTrue:[^nil].
	code > 256 ifTrue:
		[[index = (code \\ 256)] assert.
		^code \\ 256].
	code >= (CodeBases at: self type) ifTrue:
		[[index = (code - (CodeBases at: self type))] assert.
		^code - (CodeBases at: self type)].
	[index = (code - self type)] assert.
	^code - self type
]

{ #category : #testing }
VariableNode >> isSelfPseudoVariable [
	"Answer if this ParseNode represents the 'self' pseudo-variable."

	^ key = 'self' or: [name = '{{self}}']
]

{ #category : #testing }
VariableNode >> isVariableNode [
	^true
]

{ #category : #testing }
VariableNode >> isVariableReference [

	^true
]

{ #category : #accessing }
VariableNode >> name [
	^ name
]

{ #category : #initialize-release }
VariableNode >> name: string [
	"Change name"

	name := string
]

{ #category : #initialize-release }
VariableNode >> name: varName index: i type: type [
	"Only used for initting instVar refs"
	^self name: varName key: varName index: i type: type
]

{ #category : #initialize-release }
VariableNode >> name: string key: object code: byte [
	"Only used for initting std variables, nil, true, false, self, etc."
	name := string.
	key := object.
	code := byte
]

{ #category : #initialize-release }
VariableNode >> name: varName key: objRef index: i type: type [
	"Only used for initting global (litInd) variables"
	^self name: varName key: objRef code: (self code: (index := i) type: type)
]

{ #category : #printing }
VariableNode >> printOn: aStream indent: level [ 

	aStream nextPutAll: name
]

{ #category : #testing }
VariableNode >> returns [
	^false 
]

{ #category : #'code generation' }
VariableNode >> sizeCodeForReturn: encoder [
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder sizeReturnSpecialLiteral: specialLiteral].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue:
		[^encoder sizeReturnReceiver].
	^super sizeCodeForReturn: encoder
]

{ #category : #'code generation' }
VariableNode >> sizeCodeForStore: encoder [
	self shouldNotImplement
]

{ #category : #'code generation' }
VariableNode >> sizeCodeForStorePop: encoder [
	self shouldNotImplement
]

{ #category : #'code generation' }
VariableNode >> sizeCodeForValue: encoder [
	self reserve: encoder.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral| "i.e. the pseudo-variables nil true & false"
			 ^encoder sizePushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder sizePushReceiver].
	code = LdThisContext ifTrue:
		[^encoder sizePushThisContext].
	self flag: 'probably superfluous'.
	^encoder sizePushInstVar: index
]

{ #category : #testing }
VariableNode >> type [
	"This code attempts to reconstruct the type from its encoding in code.
		This allows one to test, for instance, (aNode type = LdInstType)."
	| type |
	code < 0 ifTrue: [^code negated].
	code >= 256 ifTrue: [^code // 256].
	type := CodeBases findFirst: [:one | code < one].
	^type = 0 ifTrue: [5] ifFalse: [type - 1]
]
