"
This superclass of most compiler/decompiler classes declares common class variables, default messages, and the code emitters for jumps. Some of the class variables are initialized here; the rest are initialized in class VariableNode.
"
Class {
	#name : #ParseNode,
	#superclass : #Object,
	#instVars : [
		'comment',
		'pc'
	],
	#classVars : [
		'Bfp',
		'BtpLong',
		'CodeBases',
		'CodeLimits',
		'DblExtDoAll',
		'Dup',
		'EndMethod',
		'EndRemote',
		'Jmp',
		'JmpLimit',
		'JmpLong',
		'LdFalse',
		'LdInstLong',
		'LdInstType',
		'LdLitIndType',
		'LdLitType',
		'LdMinus1',
		'LdNil',
		'LdSelf',
		'LdSuper',
		'LdTempType',
		'LdThisContext',
		'LdTrue',
		'LoadLong',
		'LongLongDoAll',
		'NodeFalse',
		'NodeNil',
		'NodeSelf',
		'NodeSuper',
		'NodeThisContext',
		'NodeTrue',
		'Pop',
		'Send',
		'SendLimit',
		'SendLong',
		'SendLong2',
		'SendPlus',
		'SendType',
		'ShortStoP',
		'StdLiterals',
		'StdSelectors',
		'StdVariables',
		'Store',
		'StorePop'
	],
	#pools : [
		'TextConstants'
	],
	#category : #Compiler-ParseNodes
}

{ #category : #accessing }
ParseNode class >> blockReturnCode [

	^ EndRemote
]

{ #category : #'class initialization' }
ParseNode class >> initialize [
	"ParseNode initialize. VariableNode initialize"
	LdInstType := 1.
	LdTempType := 2.
	LdLitType := 3.
	LdLitIndType := 4.
	SendType := 5.
	CodeBases := #(0 16 32 64 208 ).
	CodeLimits := #(16 16 32 32 16 ).
	LdSelf := 112.
	LdTrue := 113.
	LdFalse := 114.
	LdNil := 115.
	LdMinus1 := 116.
	LoadLong := 128.
	Store := 129.
	StorePop := 130.
	ShortStoP := 96.
	SendLong := 131.
	DblExtDoAll := 132.
	SendLong2 := 134.
	LdSuper := 133.
	Pop := 135.
	Dup := 136.
	LdThisContext := 137.
	EndMethod := 124.
	EndRemote := 125.
	Jmp := 144.
	Bfp := 152.
	JmpLimit := 8.
	JmpLong := 164.  "code for jmp 0"
	BtpLong := 168.
	SendPlus := 176.
	Send := 208.
	SendLimit := 16
]

{ #category : #accessing }
ParseNode class >> popCode [

	^ Pop
]

{ #category : #accessing }
ParseNode class >> pushNilCode [

	^LdNil
]

{ #category : #accessing }
ParseNode class >> tempSortBlock [
	"Answer a block that can sort a set of temporaries into a stable
	 order so that different compilations produce the same results."
	^[:t1 :t2| | be1 be2 bs1 bs2 |
	   t1 index < t2 index "simple sort by index."
	   or: [t1 index = t2 index "complex tie break" 
		  and: [t1 isRemote ~= t2 isRemote
				ifTrue: [t2 isRemote] "put direct temps before indirect temps"
				ifFalse: 
					[((be1 := t1 definingScope blockExtent) isNil
					  or: [(be2 := t2 definingScope blockExtent) isNil])
						ifTrue: [t1 name < t2 name] "only have the name left to go on"
						ifFalse: "put temps from outer scopes before those from inner scopes"
							[(bs1 := be1 first) < (bs2 := be2 first)
							 or: [bs1 = bs2 and: [t1 name < t2 name]]]]]]] "only have the name left to go on"
]

{ #category : #visiting }
ParseNode >> accept: aVisitor [
	"Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:.
	 All such implementations under ParseNode should answer the result of the dispatch, e.g.
		^aVisitor visitBlockNode: self"
	^self subclassResponsibility
]

{ #category : #converting }
ParseNode >> asReturnNode [

	^ReturnNode new expr: self
]

{ #category : #testing }
ParseNode >> assignmentCheck: encoder at: location [
	"For messageNodes masquerading as variables for the debugger.
	For now we let this through - ie we allow stores ev
	into args.  Should check against numArgs, though."
	^ -1
]

{ #category : #testing }
ParseNode >> canCascade [

	^false
]

{ #category : #comment }
ParseNode >> comment [

	^comment
]

{ #category : #comment }
ParseNode >> comment: newComment [

	comment := newComment
]

{ #category : #'code generation' }
ParseNode >> emitCodeForBlockValue: stack encoder: encoder [
	"Generate code for evaluating the last statement in a block"
	^self emitCodeForValue: stack encoder: encoder
]

{ #category : #'code generation' }
ParseNode >> emitCodeForBranchOn: condition dist: dist pop: stack encoder: encoder [
	stack pop: 1.
	dist = 0 ifTrue: [^encoder genPop].
	condition
		ifTrue: [encoder genBranchPopTrue: dist]
		ifFalse: [encoder genBranchPopFalse: dist]
]

{ #category : #'code generation' }
ParseNode >> emitCodeForEffect: stack encoder: encoder [

	self emitCodeForValue: stack encoder: encoder.
	encoder genPop.
	stack pop: 1
]

{ #category : #'code generation' }
ParseNode >> emitCodeForJump: dist encoder: encoder [

	dist = 0 ifFalse: [encoder genJump: dist]
]

{ #category : #'code generation' }
ParseNode >> emitCodeForReturn: stack encoder: encoder [

	self emitCodeForValue: stack encoder: encoder.
	encoder genReturnTop
]

{ #category : #encoding }
ParseNode >> encodeSelector: aSelector [

	^nil
]

{ #category : #testing }
ParseNode >> ensureCanCascade: encoder [
]

{ #category : #private }
ParseNode >> ifNilReceiver [
	"assuming this object is the receiver of an ifNil:, what object is being asked about?"
	^self
]

{ #category : #testing }
ParseNode >> isArg [

	^false
]

{ #category : #testing }
ParseNode >> isAssignmentNode [
	^false
]

{ #category : #testing }
ParseNode >> isBlockNode [
	^false
]

{ #category : #testing }
ParseNode >> isComplex [
	"Used for pretty printing to determine whether to start a new line"

	^false
]

{ #category : #testing }
ParseNode >> isConstantNumber [  "Overridden in LiteralNode"
	^false
]

{ #category : #testing }
ParseNode >> isDoIt [
	"polymorphic with RBNodes; called by debugger"

	^ false
]

{ #category : #testing }
ParseNode >> isJust: node [
	^false
]

{ #category : #testing }
ParseNode >> isLiteralNode [
	^false
]

{ #category : #testing }
ParseNode >> isMessage [
	^false
]

{ #category : #testing }
ParseNode >> isMessage: selSymbol receiver: rcvrPred arguments: argsPred [
	"See comment in MessageNode."

	^false
]

{ #category : #testing }
ParseNode >> isMessageNode [
	^false
]

{ #category : #testing }
ParseNode >> isOnlySubnodeOf: aSubtree "<ParseNode>" in: aParseTree [ "<ParseNode>"
	"Answer if the receiver only occurs within aSubtree of aParseTree, not in the rest of aParseTree.
	 Assumes that aSubtree is in fact a subnode of aParseTree."
	| isSubnode |
	isSubnode := false.
	aSubtree accept: (ParseNodeEnumerator
							ofBlock: [:node| node == self ifTrue: [isSubnode := true]]).
	isSubnode ifFalse:
		[^false].
	aParseTree accept: (ParseNodeEnumerator
							ofBlock: [:node| node == self ifTrue: [^false]]
							select: [:node| node ~= aSubtree]).
	^true
]

{ #category : #testing }
ParseNode >> isReturn [
	^false
]

{ #category : #testing }
ParseNode >> isReturnSelf [

	^false
]

{ #category : #testing }
ParseNode >> isReturningIf [

	^false
]

{ #category : #testing }
ParseNode >> isSelfPseudoVariable [	
	"Overridden in VariableNode."
	^false
]

{ #category : #testing }
ParseNode >> isSpecialConstant [
	^ false
]

{ #category : #testing }
ParseNode >> isTemp [
	^ false
]

{ #category : #testing }
ParseNode >> isUndefTemp [
	^ false
]

{ #category : #testing }
ParseNode >> isUnusedTemp [
	^ false
]

{ #category : #testing }
ParseNode >> isVariableNode [
	^false
]

{ #category : #testing }
ParseNode >> isVariableReference [

	^false
]

{ #category : #private }
ParseNode >> nextWordFrom: aStream setCharacter: aBlock [
	| outStream char |
	outStream := (String new: 16) writeStream.
	[(aStream peekFor: Character space) 
		or: [aStream peekFor: Character tab]] whileTrue.
	[aStream atEnd
		or:
			[char := aStream next.
			char = Character cr or: [char = Character space]]]
		whileFalse: [outStream nextPut: char].
	aBlock value: char.
	^ outStream contents
]

{ #category : #printing }
ParseNode >> nodePrintOn: aStrm indent: nn [
	| var aaStrm myLine |
	"Show just the sub nodes and the code."

	(aaStrm := aStrm) ifNil: [aaStrm := (String new: 500) writeStream].
	nn timesRepeat: [aaStrm tab].
	aaStrm nextPutAll: self class name; space.
	myLine := self printString copyWithout: Character cr.
	myLine := myLine copyFrom: 1 to: (myLine size min: 70).
	aaStrm nextPutAll: myLine; cr.
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var respondsTo: #asReturnNode) ifTrue: [var nodePrintOn: aaStrm indent: nn+1]].
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var isKindOf: SequenceableCollection) ifTrue: [
				var do: [:aNode | 
					(aNode respondsTo: #asReturnNode) ifTrue: [
						aNode nodePrintOn: aaStrm indent: nn+1]]]].
	^ aaStrm

]

{ #category : #visiting }
ParseNode >> nodesDo: aBlock [
	self accept: (ParseNodeEnumerator ofBlock: aBlock)
]

{ #category : #testing }
ParseNode >> nowHasDef [  "Ignored in all but VariableNode"
]

{ #category : #testing }
ParseNode >> nowHasRef [  "Ignored in all but VariableNode"
]

{ #category : #'code generation' }
ParseNode >> pc [
	"Used by encoder source mapping."

	^pc ifNil: [ 0 ]

]

{ #category : #'code generation' }
ParseNode >> pc: anInteger [
	"Used by encoder source mapping."

	pc := anInteger
]

{ #category : #printing }
ParseNode >> printAsIfCompiledOn: aStream [ 
	"Refer to the comment in Object|printOn:."
	self printOn: aStream indent: 0.

]

{ #category : #printing }
ParseNode >> printCommentOn: aStream indent: indent [ 
	| thisComment |
	self comment == nil ifTrue: [^ self].
	1 to: self comment size
	   do: [:index |
		index > 1 ifTrue: [aStream crtab: indent].
		aStream nextPut: $".
		thisComment := self comment at: index.
		self printSingleComment: thisComment
			on: aStream
			indent: indent.
		aStream nextPut: $"]
]

{ #category : #printing }
ParseNode >> printOn: aStream [ 
	"Refer to the comment in Object|printOn:."

	aStream nextPut: ${.
	self printAsIfCompiledOn: aStream.
	aStream nextPut: $}.
]

{ #category : #printing }
ParseNode >> printOn: aStream indent: anInteger [ 
	"If control gets here, avoid recursion loop."

	super printOn: aStream
]

{ #category : #printing }
ParseNode >> printOn: aStream indent: level precedence: p [

	self printOn: aStream indent: level
]

{ #category : #private }
ParseNode >> printSingleComment: aString on: aStream indent: indent [ 
	"Print the comment string, assuming it has been indented indent tabs.
	Break the string at word breaks, given the widths in the default
	font, at 450 points."

	| readStream word position lineBreak wordWidth tabWidth spaceWidth lastChar |
	readStream := aString readStream.
	tabWidth := 24."Real value to remove the access to Text-Core"
	spaceWidth := 11."abstract value to remove the access to font. Should be suffisant"
	position := indent * tabWidth.
	lineBreak := 450.
	[readStream atEnd]
		whileFalse: 
			[word := self nextWordFrom: readStream setCharacter: [:lc | lastChar := lc].
			wordWidth := word inject: 0 into: [:width :char | width + 11 "abstract value to remove the access to font. Should be suffisant. In any case, this method should be removed in a near future..."].
			position := position + wordWidth.
			position > lineBreak
				ifTrue: 
					[aStream skip: -1; crtab: indent.
					position := indent * tabWidth + wordWidth + spaceWidth.
					lastChar = Character cr
						ifTrue: [[readStream peekFor: Character tab] whileTrue].
					word isEmpty ifFalse: [aStream nextPutAll: word; space]]
				ifFalse: 
					[aStream nextPutAll: word.
					readStream atEnd
						ifFalse: 
							[position := position + spaceWidth.
							aStream space].
					lastChar = Character cr
						ifTrue: 
							[aStream skip: -1; crtab: indent.
							position := indent * tabWidth.
							[readStream peekFor: Character tab] whileTrue]]]
]

{ #category : #printing }
ParseNode >> shortPrintOn: aStream [ 
	self printOn: aStream indent: 0
]

{ #category : #'code generation' }
ParseNode >> sizeCode: encoder forBranchOn: condition dist: dist [
	dist = 0 ifTrue: [^encoder sizePop].
	^condition
		ifTrue: [encoder sizeBranchPopTrue: dist]
		ifFalse: [encoder sizeBranchPopFalse: dist]
]

{ #category : #'code generation' }
ParseNode >> sizeCode: encoder forJump: dist [

	^dist = 0 ifTrue: [0] ifFalse: [encoder sizeJump: dist]
]

{ #category : #'code generation' }
ParseNode >> sizeCodeForBlockValue: encoder [
	"Answer the size for evaluating the last statement in a block"
	^self sizeCodeForValue: encoder
]

{ #category : #'code generation' }
ParseNode >> sizeCodeForEffect: encoder [

	^(self sizeCodeForValue: encoder) + encoder sizePop
]

{ #category : #'code generation' }
ParseNode >> sizeCodeForReturn: encoder [

	^(self sizeCodeForValue: encoder) + encoder sizeReturnTop
]

{ #category : #testing }
ParseNode >> toDoIncrement: ignored [
	"Only meant for Messages or Assignments - else return nil"
	^ nil
]
