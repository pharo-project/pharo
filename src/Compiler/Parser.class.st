"
I parse Smalltalk syntax and create a MethodNode that is the root of the parse tree. I look one token ahead.
"
Class {
	#name : #Parser,
	#superclass : #Scanner,
	#instVars : [
		'here',
		'hereType',
		'hereMark',
		'hereEnd',
		'prevMark',
		'prevEnd',
		'encoder',
		'requestor',
		'parseNode',
		'failBlock',
		'requestorOffset',
		'tempsMark',
		'doitFlag',
		'properties',
		'category'
	],
	#category : #'Compiler-Kernel'
}

{ #category : #accessing }
Parser class >> primitivePragmaSelectors [
	"Answer the selectors of pragmas that specify VM primitives.
	 Needed for compile and decomple."

	^ (Pragma allNamed: #primitive from: self to: Parser)
		collect: [ :each | each methodSelector ]
]

{ #category : #private }
Parser >> addComment [

	parseNode ~~ nil
		ifTrue: 
			[parseNode comment: currentComment.
			currentComment := nil]
]

{ #category : #pragmas }
Parser >> addPragma: aPragma [
	properties := properties copyWith: aPragma
]

{ #category : #'error handling' }
Parser >> addWarning: aString [
	"ignored by the default compiler."
]

{ #category : #scanning }
Parser >> advance [
	| this |
	prevMark := hereMark.
	prevEnd := hereEnd.
	this := here.
	here := token.
	hereType := tokenType.
	hereMark := mark.
	hereEnd := source position - (aheadChar == DoItCharacter
		ifTrue: [hereChar == DoItCharacter
			ifTrue: [0]
			ifFalse: [1]]
		ifFalse: [2]).
	self scanToken.
	"Transcript show: 'here: ', here printString, ' mark: ', hereMark printString, ' end: ', hereEnd printString; cr."
	^this
]

{ #category : #primitives }
Parser >> allocateLiteral: lit [
	encoder litIndex: lit
]

{ #category : #'error correction' }
Parser >> ambiguousSelector: aString inRange: anInterval [
	| correctedSelector userSelection offset intervalWithOffset |
	
	self interactive ifFalse: [
		"In non interactive mode, break backward compatibility: $- is part of selector"
		Transcript cr; nextPutAll: encoder classEncoding storeString; nextPutAll:#'>>'; nextPutAll: encoder selector storeString; show: ' will send ' , token , '-'.
		^super ambiguousSelector: aString inRange: anInterval].
	
	"handle the text selection"
	userSelection := requestor selectionInterval.
	intervalWithOffset := anInterval first + requestorOffset to: anInterval last + requestorOffset.
	requestor selectFrom: intervalWithOffset first to: intervalWithOffset last.
	requestor select.

	"Build the menu with alternatives"
	correctedSelector := AmbiguousSelectorDetected 
			signalName: aString
			inRange: intervalWithOffset.
	correctedSelector ifNil: [^self fail].

	"Execute the selected action"
	offset := self substituteWord: correctedSelector wordInterval: intervalWithOffset offset: 0.
	requestor deselect.
	requestor selectInvisiblyFrom: userSelection first to: userSelection last + offset.
	token := (correctedSelector readStream upTo: Character space) asSymbol
]

{ #category : #'expression types' }
Parser >> argumentName [

	hereType == #word
		ifFalse: [^self expected: 'Argument name'].
	^self advance
]

{ #category : #'expression types' }
Parser >> assignment: varNode [
	" var ':=' expression => AssignmentNode."
	| loc start |
	(loc := varNode assignmentCheck: encoder at: prevMark + requestorOffset) >= 0
		ifTrue: [^self notify: 'Cannot store into' at: loc].
	start := self startOfNextToken.
	self advance.
	self expression ifFalse: [^self expected: 'Expression'].
	parseNode := AssignmentNode new
				variable: varNode
				value: parseNode
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	varNode nowHasDef.
	^true
]

{ #category : #temps }
Parser >> bindArg: name [

	^ self bindTemp: name
]

{ #category : #temps }
Parser >> bindTemp: name [

	^name
]

{ #category : #temps }
Parser >> bindTemp: name in: methodSelector [

	^name
]

{ #category : #'expression types' }
Parser >> blockExpression [
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode variableNodes temporaryBlockVariables start |
	blockNode := BlockNode new.
	variableNodes := OrderedCollection new.
	start := prevMark + requestorOffset.
	"Gather parameters."
	[self match: #colon] whileTrue:
		[variableNodes addLast: (encoder bindBlockArg: self argumentName within: blockNode)].
	(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
		[^self expected: 'Vertical bar'].

	temporaryBlockVariables := self temporaryBlockVariablesFor: blockNode.
	self statements: variableNodes innerBlock: true blockNode: blockNode.
	blockNode temporaries: temporaryBlockVariables.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	temporaryBlockVariables do: [:variable | variable scope: -1].
	variableNodes do: [:variable | variable scope: -1]
]

{ #category : #'expression types' }
Parser >> braceExpression [
	" { elements } => BraceNode."

	| elements locations loc more |
	elements := OrderedCollection new.
	locations := OrderedCollection new.
	self advance.
	more := hereType ~~ #rightBrace.
	[more]
		whileTrue: 
			[loc := hereMark + requestorOffset.
			self expression
				ifTrue: 
					[elements addLast: parseNode.
					locations addLast: loc]
				ifFalse:
					[^self expected: 'Variable or expression'].
			(self match: #period)
				ifTrue: [more := hereType ~~ #rightBrace]
				ifFalse: [more := false]].
	parseNode := BraceNode new elements: elements sourceLocations: locations.
	(self match: #rightBrace)
		ifFalse: [^self expected: 'Period or right brace'].
	^true
]

{ #category : #'error correction' }
Parser >> canDeclareClassVariable [
	^encoder classEncoding ~~ UndefinedObject
]

{ #category : #'expression types' }
Parser >> cascade [
	" {; message} => CascadeNode."

	| rcvr msgs |
	parseNode canCascade ifFalse:
		[^self expected: 'Cascading not'].
	parseNode ensureCanCascade: encoder.
	rcvr := parseNode cascadeReceiver.
	msgs := OrderedCollection with: parseNode.
	[self match: #semicolon]
		whileTrue: 
			[parseNode := rcvr.
			(self messagePart: 3 repeat: false)
				ifFalse: [^self expected: 'Cascade'].
			parseNode canCascade ifFalse:
				[^self expected: '<- No special messages'].
			parseNode ensureCanCascade: encoder.
				parseNode cascadeReceiver.
			msgs addLast: parseNode].
	parseNode := CascadeNode new receiver: rcvr messages: msgs
]

{ #category : #'error correction' }
Parser >> correctSelector: proposedKeyword wordIntervals: spots exprInterval: expInt ifAbort: abortAction [
	"Correct the proposedKeyword to some selector symbol, correcting the original text if such action is indicated.  abortAction is invoked if the proposedKeyword couldn't be converted into a valid selector.  Spots is an ordered collection of intervals within the test stream of the for each of the keyword parts."

	| correctSelector userSelection |
	"If we can't ask the user, assume that the keyword will be defined later"
	(self interactive and: [ self warns ])
		ifFalse: [^proposedKeyword asSymbol].

	userSelection := requestor selectionInterval.
	requestor selectFrom: spots first first to: spots last last.
	requestor select.

	correctSelector := UnknownSelectorDetected name: proposedKeyword.
	correctSelector ifNil: [^abortAction value].

	requestor deselect.
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	self substituteSelector: correctSelector keywords wordIntervals: spots.
	^(proposedKeyword last ~~ $:
	   and: [correctSelector last == $:])
		ifTrue: [abortAction value]
		ifFalse: [correctSelector]
]

{ #category : #'error correction' }
Parser >> correctVariable: proposedVariable interval: spot [ 
	"Correct the proposedVariable to a known variable, or declare it as a new
	variable if such action is requested.  We support declaring lowercase
	variables as temps or inst-vars, and uppercase variables as Globals or 
	ClassVars, depending on whether the context is nil (class=UndefinedObject).
	Spot is the interval within the test stream of the variable. "

	"Check if this is an i-var, that has been corrected already (ugly)"

	"Display the pop-up menu"

	| tempIvar binding userSelection action |
	(encoder classEncoding instVarNames includes: proposedVariable) ifTrue: 
		[^InstanceVariableNode new 
			name: proposedVariable
			index: (encoder classEncoding allInstVarNames indexOf: proposedVariable)].

	"If we can't ask the user for correction, make it undeclared"
	self interactive ifFalse: [^encoder undeclared: proposedVariable].

	"First check to see if the requestor knows anything about the variable"
	tempIvar := proposedVariable first isLowercase.
	(tempIvar and: [(binding := requestor bindingOf: proposedVariable) notNil]) 
		ifTrue: [^encoder global: binding name: proposedVariable].
	userSelection := requestor selectionInterval.
	requestor selectFrom: spot first to: spot last.
	"requestor select."

	"Build the menu with alternatives"
	action := UndeclaredVariableDetected 
				signalFor: self
				name: proposedVariable
				inRange: spot.
	action ifNil: [^self fail].

	"Execute the selected action"
	requestor deselect.
	requestor selectInvisiblyFrom: userSelection first to: userSelection last.

	^action value
]

{ #category : #'error correction' }
Parser >> declareClassVar: name [
	| sym class |
	sym := name asSymbol.
	class := encoder classEncoding.
	class := class instanceSide.		"not the metaclass"
	class addClassVarNamed: name.
	^ encoder global: (class classPool associationAt: sym)
			name: sym
]

{ #category : #'error correction' }
Parser >> declareGlobal: name [
	| sym |
	sym := name asSymbol.
	^encoder
		global: (encoder environment
					at: sym put: nil;
					associationAt: sym)
		name: sym
]

{ #category : #'error correction' }
Parser >> declareInstVar: name [
	"Declare an instance variable.  Since the variable will get added after any existing
	 inst vars its index is the instSize."
	encoder classEncoding addInstVarNamed: name.
	^InstanceVariableNode new name: name index: encoder classEncoding instSize
		
]

{ #category : #'error correction' }
Parser >> declareTempAndPaste: name [
	| insertion delta theTextString characterBeforeMark |

	theTextString := requestor text string.
	characterBeforeMark := theTextString at: tempsMark-1 ifAbsent: [$ ].
	(theTextString at: tempsMark) = $| ifTrue: [
  		"Paste it before the second vertical bar"
		insertion := name, ' '.
		characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].
		delta := 0.
	] ifFalse: [
		"No bars - insert some with CR, tab"
		insertion := '| ' , name , ' |',String cr.
		delta := 2.	"the bar and CR"
		characterBeforeMark = Character tab ifTrue: [
			insertion := insertion , String tab.
			delta := delta + 1.	"the tab"
		].
	].
	tempsMark := tempsMark +
		(self substituteWord: insertion
			wordInterval: (tempsMark to: tempsMark-1)
			offset: 0) - delta.
	^ encoder bindAndJuggle: name
]

{ #category : #'error correction' }
Parser >> defineClass: className [ 
	"Prompts the user to define a new class."
	
	| classSymbol systemCategory classDefinition |
	classSymbol := className asSymbol.
	systemCategory := self encoder classEncoding instanceSide category
		ifNil: [ 'Unknown' ].
	classDefinition := 'Object subclass: #' , classSymbol , '
		instanceVariableNames: '''' 
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''' , systemCategory , ''''.
	classDefinition := UIManager default 
		multiLineRequest: 'Edit class definition:'
		initialAnswer: classDefinition
		answerHeight: 150.
	(classDefinition isNil or: [ classDefinition isEmpty ])
		ifTrue: [ ^ self fail ].
	self class compiler evaluate: classDefinition.
	^ encoder
		global: (Smalltalk globals associationAt: classSymbol)
		name: classSymbol
]

{ #category : #'public access' }
Parser >> encoder [
	^encoder ifNil:
		[encoder := LegacyEncoderForV3PlusClosures new]
]

{ #category : #'public access' }
Parser >> encoderClass: anEncoderClass [
	encoder ifNotNil: [
		self error: 'encoder already set'].
	encoder := anEncoderClass new
]

{ #category : #scanning }
Parser >> endOfLastToken [

	^ prevEnd ifNil: [mark]
]

{ #category : #'error handling' }
Parser >> expected: aString [ 
	"Notify a problem at token 'here'."

	^ self notify: aString , ' expected' at: hereMark + requestorOffset
]

{ #category : #'expression types' }
Parser >> expression [

	(hereType == #word and: [tokenType == #leftArrow])
		ifTrue: [^ self assignment: self variable].
	hereType == #leftBrace
		ifTrue: [self braceExpression]
		ifFalse: [self primaryExpression ifFalse: [^ false]].
	(self messagePart: 3 repeat: true)
		ifTrue: [hereType == #semicolon ifTrue: [self cascade]].
	^ true
]

{ #category : #primitives }
Parser >> externalFunctionDeclaration [
	"Parse the function declaration for a call to an external library."

	| descriptorClass callType retType externalName args argType module |
	descriptorClass := Smalltalk globals at: #ExternalFunction ifAbsent: [ nil ].
	descriptorClass == nil 
		ifTrue: [ ^ false ].
	callType := descriptorClass callingConventionFor: here.
	callType == nil 
		ifTrue: [ ^ false ].	"Parse return type"
	self advance.
	retType := self externalType: descriptorClass.
	retType == nil 
		ifTrue: [ ^ self expected: 'return type' ].	"Parse function name or index"
	externalName := here.
	(self match: #string)
		ifTrue: [ externalName := externalName asSymbol ]
		ifFalse: [ (self match: #number)
						ifFalse: [ ^ self expected: 'function name or index' ] ].
	(self matchToken: #'(')
		ifFalse: [ ^ self expected: 'argument list' ].
	args := Array new writeStream.
	[ here == #')' ]
		whileFalse: [ 
			argType := self externalType: descriptorClass.
			argType == nil
				ifTrue: [ ^ self expected: 'argument' ].
			argType isVoid & argType isPointerType not
				ifFalse: [ args nextPut: argType ] ].
	(args position = self properties selector numArgs) ifFalse: [
		^self expected: 'Matching number of arguments'].
	(self matchToken: #')')
		ifFalse: [ ^ self expected: ')' ].
	(self matchToken: 'module:')
		ifTrue: [ 
			module := here.
			(self match: #string)
				ifFalse: [ ^ self expected: 'String' ].
			module := module asSymbol ].
	Smalltalk globals
		at: #ExternalLibraryFunction
		ifPresent: [ :xfn | 
			| fn |
			fn := xfn
				name: externalName
				module: module
				callType: callType
				returnType: retType
				argumentTypes: args contents.
			self allocateLiteral: fn ].
	self addPragma: (Pragma keyword: #primitive: arguments: #(120)).
	^ true
]

{ #category : #primitives }
Parser >> externalType: descriptorClass [
	"Parse an return an external type"
	| xType |
	xType := descriptorClass atomicTypeNamed: here.
	xType == nil ifTrue:["Look up from class scope"
		Symbol hasInterned: here ifTrue:[:sym|
			xType := descriptorClass structTypeNamed: sym]].
	xType == nil ifTrue:[
		"Raise an error if user is there"
		self interactive ifTrue:[^nil].
		"otherwise go over it silently"
		xType := descriptorClass forceTypeNamed: here].
	self advance.
	(self matchToken:#*)
		ifTrue:[^xType asPointerType]
		ifFalse:[^xType]
]

{ #category : #'error handling' }
Parser >> fail [

	| exitBlock |
	(encoder isNil or: [encoder == self])
		ifFalse: [encoder release. encoder := nil]. "break cycle"
	exitBlock := failBlock.
	failBlock := nil.
	^exitBlock value
]

{ #category : #private }
Parser >> init: sourceStream notifying: req failBlock: aBlock [

	requestor := req.
	failBlock := aBlock.
	requestorOffset := 0.
	super scan: sourceStream.
	prevMark := hereMark := mark.
	self advance
]

{ #category : #private }
Parser >> initPattern: aString notifying: req return: aBlock [

	| result |
	self
		init: aString asString readStream
		notifying: req
		failBlock: [^nil].
	encoder := self.
	result := aBlock value: (self pattern: false inContext: nil).
	encoder := failBlock := nil.  "break cycles"
	^result
]

{ #category : #'error handling' }
Parser >> interactive [
	requestor ifNil: [^ false ].
	^(requestor respondsTo: #interactive)
		ifTrue: [ requestor interactive ]
		ifFalse: [ true ]
]

{ #category : #scanning }
Parser >> match: type [ 
	"Answer with true if next tokens type matches."

	hereType == type
		ifTrue: 
			[self advance.
			^true].
	^false
]

{ #category : #scanning }
Parser >> matchReturn [

	^ self match: #upArrow
]

{ #category : #scanning }
Parser >> matchToken: thing [ 
	"Matches the token, not its type."

	here = thing ifTrue: [self advance. ^true].
	^false
]

{ #category : #'expression types' }
Parser >> messagePart: level repeat: repeat [

	| start receiver selector args precedence words keywordStart |
	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: 
			[start := self startOfNextToken.
			selector := (String new: 32) writeStream.
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue: 
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [((hereType == #binary or: [hereType == #verticalBar])
				and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asOctetString asSymbol.
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue: 
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].
	parseNode := MessageNode new
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	repeat]
		whileTrue: [].
	^true
]

{ #category : #'expression types' }
Parser >> method: doit context: ctxt [
	" pattern [ | temporaries ] block => MethodNode."

	| sap blk prim temps messageComment methodNode |
	properties := AdditionalMethodState new.
	sap := self pattern: doit inContext: ctxt.
	"sap={selector, arguments, precedence}"
	properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	doit ifFalse: [self pragmaSequence].
	temps := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	doit ifFalse: [self pragmaSequence].
	prim := self pragmaPrimitives.
	self statements: #() innerBlock: doit.
	blk := parseNode.
	doit ifTrue: [blk returnLast]
		ifFalse: [blk returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	self interactive ifTrue: [self removeUnusedTemps].
	methodNode := self newMethodNode comment: messageComment.
	^methodNode
		selector: (sap at: 1)
		arguments: (sap at: 2)
		precedence: (sap at: 3)
		temporaries: temps
		block: blk
		encoder: encoder
		primitive: prim
		properties: properties
]

{ #category : #'expression types' }
Parser >> newMethodNode [
	^self encoder methodNodeClass new
]

{ #category : #'error handling' }
Parser >> notify: aString [ 
	"Notify problem at token before 'here'."

	^self notify: aString at: prevMark + requestorOffset
]

{ #category : #'error handling' }
Parser >> notify: string at: location [
	requestor isNil
		ifTrue: [(encoder == self or: [encoder isNil]) ifTrue: [^ self fail  "failure setting up syntax error"].
				SyntaxErrorNotification
					inClass: encoder classEncoding
					withCode: source contents
					doitFlag: doitFlag
					errorMessage: string
					location: location]
		ifFalse: [requestor
					notify: string , ' ->'
					at: location
					in: source].
	^self fail
]

{ #category : #'error handling' }
Parser >> offEnd: aString [ 
	"Notify a problem beyond 'here' (in lookAhead token). Don't be offEnded!"

	requestorOffset == nil
		ifTrue: [^ self notify: aString at: mark]
		ifFalse: [^ self notify: aString at: mark + requestorOffset]

]

{ #category : #'public access' }
Parser >> parse: sourceStreamOrString class: behavior [

	^ self parse: sourceStreamOrString readStream class: behavior
		noPattern: false context: nil notifying: nil ifFail: [ self fail ]
]

{ #category : #'public access' }
Parser >> parse: sourceStream class: class category: aCategory noPattern: noPattern context: ctxt notifying: req ifFail: aBlock [ 
	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."

	| methNode repeatNeeded myStream s p |
	category := aCategory.
	myStream := sourceStream.
	[repeatNeeded := false.
	 p := myStream position.
	 s := myStream upToEnd.
	 myStream position: p.
	self encoder init: class context: ctxt notifying: self.
	 self init: myStream notifying: req failBlock: [^ aBlock value].
	 doitFlag := noPattern.
	 failBlock:= aBlock.
	 [methNode := self
					method: noPattern
					context: ctxt] 
		on: ReparseAfterSourceEditing 
		do:	[ :ex |
			repeatNeeded := true.
			myStream := requestor text string readStream].
	 repeatNeeded] whileTrue:
		[encoder := self encoder class new].
	requestor 
		ifNil: [ methNode sourceText: s ]
		ifNotNil: [ methNode sourceText: requestor text string ].
	^methNode

]

{ #category : #'public access' }
Parser >> parse: sourceStream class: class noPattern: noPattern context: ctxt notifying: req ifFail: aBlock [ 
	^self parse: sourceStream class: class category: nil noPattern: noPattern context: ctxt notifying: req ifFail: aBlock 
]

{ #category : #'public access' }
Parser >> parseSelector: aString [ 
	"Answer the message selector for the argument, aString, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."

	self initScannerForTokenization.
	^self
		initPattern: aString
		notifying: nil
		return: [:pattern | pattern at: 1]
]

{ #category : #'expression types' }
Parser >> pattern: fromDoit inContext: ctxt [
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	| args selector |
	doitFlag := fromDoit.
	fromDoit ifTrue:
		[^ctxt == nil
			ifTrue: [{#DoIt. {}. 1}]
			ifFalse: [{#DoItIn:. {encoder encodeVariable: encoder doItInContextName}. 3}]].

	hereType == #word ifTrue: [^ {self advance asSymbol. {}. 1}].

	(hereType == #binary or: [hereType == #verticalBar]) ifTrue: 
		[selector := self advance asSymbol.
		args := Array with: (encoder bindArg: self argumentName).
		^ {selector. args. 2}].

	hereType == #keyword ifTrue: 
		[selector := (String new: 32) writeStream.
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue:[
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ {selector contents asSymbol. args. 3}].
	^self expected: 'Message pattern'
]

{ #category : #'error correction' }
Parser >> possibleVariablesFor: proposedVariable [ 
	^encoder possibleVariablesFor: proposedVariable
]

{ #category : #pragmas }
Parser >> pragmaLiteral: selectorSoFar [
	"Read a pragma literal.  As a nicety we allow a variable name (rather
	 than a literal string) as the second argument to primitive:error:"

	(hereType == #string or: [ hereType == #literal or: [ hereType == #number ] ])
		ifTrue: [ ^ self advance ].
	(here == $# and: [ tokenType == #word ])
		ifTrue: [ ^ self advance ].
	(here == #- and: [ tokenType == #number ])
		ifTrue: [ ^ (self advance; advance) negated ].
	(here = 'true' or: [ here = 'false' or: [ here = 'nil' ] ])
		ifTrue: [ ^ self class compiler evaluate: self advance ].
	"This nicety allows one to supply a primitive error
	 temp as a variable name, rather than a string."
	((selectorSoFar beginsWith: 'primitive:')
	 and: [(selectorSoFar endsWith: 'error:')
	 and: [hereType == #word]]) ifTrue:
		[^self advance].
	^self expected: 'Literal constant'
]

{ #category : #pragmas }
Parser >> pragmaPrimitives [
	| primitiveSelectors primitives |
	properties isEmpty ifTrue:
		[^0].
	primitiveSelectors := self class primitivePragmaSelectors.
	primitives := properties pragmas select:
					[:pragma|
					primitiveSelectors includes: pragma keyword].
	primitives isEmpty ifTrue:
		[^0].
	primitives size > 1 ifTrue:
		[^self notify: 'Ambigous primitives'].
	^self perform: primitives first keyword withArguments: primitives first arguments
]

{ #category : #pragmas }
Parser >> pragmaSequence [
	"Parse a sequence of method pragmas."
	
	[ true ] whileTrue: [
		(self matchToken: #<)
			ifFalse: [ ^ self ].
		self pragmaStatement.
		(self matchToken: #>)
			ifFalse: [ ^ self expected: '>' ] ]
]

{ #category : #pragmas }
Parser >> pragmaStatement [
	"Read a single pragma statement. Parse all generic pragmas in the form of: <key1: val1 key2: val2 ...> and remember them, including primitives."
	
	| selector arguments words index keyword |
	(hereType = #keyword or: [ hereType = #word or: [ hereType = #binary ] ])
		ifFalse: [  ^ self expected: 'pragma declaration' ].

	" This is a ugly hack into the compiler of the FFI package. FFI should be changed to use propre pragmas that can be parsed with the code here. "
	(here = #apicall: or: [ here = #cdecl: ])
		ifTrue: [ ^ self externalFunctionDeclaration ].

	selector := String new.
	arguments := OrderedCollection new.
	words := OrderedCollection new.
	[ hereType = #keyword or: [ (hereType = #word or: [ hereType = #binary ]) and: [ selector isEmpty ] ] ] whileTrue: [
		index := self startOfNextToken + requestorOffset.
		selector := selector , self advance.
		words add: (index to: self endOfLastToken + requestorOffset).
		(selector last = $: or: [ selector first isLetter not ])
			ifTrue: [ arguments add: (self pragmaLiteral: selector) ] ].
	selector numArgs ~= arguments size
		ifTrue: [ ^ self expected: 'pragma argument' ].
	(Symbol hasInterned: selector 
		ifTrue: [ :value | keyword := value]) 
		ifFalse: [ 
			keyword := self 
				correctSelector: selector wordIntervals: words
				exprInterval: (words first first to: words last last)
				ifAbort: [ ^ self fail ] ].
	self addPragma: (Pragma keyword: keyword arguments: arguments asArray).
	^ true
]

{ #category : #'expression types' }
Parser >> primaryExpression [ 
	hereType == #word 
		ifTrue: 
			[parseNode := self variable.
			(parseNode isUndefTemp and: [self interactive])
				ifTrue: [ self warns ifTrue: [self queryUndefined]].
			parseNode nowHasRef.
			^ true].
	hereType == #leftBracket
		ifTrue: 
			[self advance.
			self blockExpression.
			^true].
	hereType == #leftBrace
		ifTrue: 
			[self braceExpression.
			^true].
	hereType == #leftParenthesis
		ifTrue: 
			[self advance.
			self expression ifFalse: [^self expected: 'expression'].
			(self match: #rightParenthesis)
				ifFalse: [^self expected: 'right parenthesis'].
			^true].
	(hereType == #string or: [hereType == #number or: [hereType == #literal]])
		ifTrue: 
			[parseNode := encoder encodeLiteral: self advance.
			^true].
	(here == #- and: [tokenType == #number and: [1 + hereEnd = mark]])
		ifTrue: 
			[self advance.
			parseNode := encoder encodeLiteral: self advance negated.
			^true].
	^false
]

{ #category : #primitives }
Parser >> primitive: anIntegerOrString [
	"Create indexed primitive."
	
	<primitive>
	^self primitive: anIntegerOrString error: nil
]

{ #category : #primitives }
Parser >> primitive: anIntegerOrString error: errorCodeVariableOrNil [
	"Create indexed primitive with optional error code."
	
	<primitive>
	^anIntegerOrString isInteger
		ifTrue:
			[errorCodeVariableOrNil ifNotNil:
				[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
			 anIntegerOrString]
		ifFalse:
			[anIntegerOrString isString
				ifTrue: [self primitive: anIntegerOrString module: nil error: errorCodeVariableOrNil]
				ifFalse: [self expected: 'Indexed primitive']]
]

{ #category : #primitives }
Parser >> primitive: aNameString error: errorCodeVariableOrNil module: aModuleStringOrNil [
	"Create named primitive with optional error code."
	
	<primitive>
	^self primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
]

{ #category : #primitives }
Parser >> primitive: aNameString module: aModuleStringOrNil [
	"Create named primitive."
	
	<primitive>
	^self primitive: aNameString module: aModuleStringOrNil error: nil
]

{ #category : #primitives }
Parser >> primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil [
	"Create named primitive with optional error code."
	
	<primitive>
	(aNameString isString and: [ aModuleStringOrNil isNil or: [ aModuleStringOrNil isString ] ])
		ifFalse: [ ^ self expected: 'Named primitive' ].
	self allocateLiteral: (Array 
		with: (aModuleStringOrNil isNil 
			ifFalse: [ aModuleStringOrNil asSymbol ])
		with: aNameString asSymbol
		with: 0 with: 0).
	errorCodeVariableOrNil ifNotNil:
		[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
	^117
]

{ #category : #pragmas }
Parser >> properties [
	^ properties ifNil: [ properties := AdditionalMethodState new ]
]

{ #category : #'error correction' }
Parser >> queryUndefined [
	| varStart varName | 
	varName := parseNode key.
	varStart := self endOfLastToken + requestorOffset - varName size + 1.
	requestor selectFrom: varStart to: varStart + varName size - 1; select.
	(UndefinedVariableDetected name: varName) ifFalse: [^ self fail]
]

{ #category : #'error correction' }
Parser >> removeUnusedTemps [
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	| str madeChanges | 
	"I disabled this option. I keep the old code just in case - Hernan Wilkinson"
	self warns ifFalse: [ ^ self ].
	
	madeChanges := false.
	str := requestor text asString.
	((tempsMark between: 1 and: str size)
		and: [(str at: tempsMark) = $|]) ifFalse: [^ self].
	encoder unusedTempNames do:
		[:temp | | start end |
		(UnusedVariableDetected name: temp) ifTrue:
			[(encoder encodeVariable: temp) isUndefTemp
				ifTrue:
					[end := tempsMark.
					["Beginning at right temp marker..."
					start := end - temp size + 1.
					end < temp size or: [temp = (str copyFrom: start to: end)
							and: [(str at: start-1) isSeparator & (str at: end+1) isSeparator]]]
						whileFalse:
							["Search left for the unused temp"
							end := requestor nextTokenFrom: end direction: -1].
					end < temp size ifFalse:
						[(str at: start-1) = $  ifTrue: [start := start-1].
						requestor correctFrom: start to: end with: ''.
						str := str copyReplaceFrom: start to: end with: ''. 
						madeChanges := true.
						tempsMark := tempsMark - (end-start+1)]]
				ifFalse:
					[self inform:
'You''ll first have to remove the\statement where it''s stored into' withCRs]]].
	madeChanges ifTrue: [ReparseAfterSourceEditing signal]
]

{ #category : #scanning }
Parser >> startOfNextToken [
	"Return starting position in source of next token."

	hereType == #doIt ifTrue: [^source position + 1].
	^hereMark
]

{ #category : #'expression types' }
Parser >> statements: argNodes innerBlock: inner [

	^self statements: argNodes innerBlock: inner blockNode: BlockNode new
]

{ #category : #'expression types' }
Parser >> statements: argNodes innerBlock: inner blockNode: theBlockNode [

	| stmts returns start |
	"give initial comment to block, since others trail statements"
	theBlockNode comment: currentComment.
	stmts := OrderedCollection new.
	returns := false.
	hereType ~~ #rightBracket ifTrue:
		[[theBlockNode startOfLastStatement: (start := self startOfNextToken).
		  (returns := self matchReturn)
			ifTrue: 
				[self expression ifFalse:
					[^self expected: 'Expression to return'].
				 self addComment.
				 stmts addLast: (parseNode isReturningIf
								ifTrue: [parseNode]
								ifFalse: [ReturnNode new
											expr: parseNode
											encoder: encoder
											sourceRange: (start to: self endOfLastToken)])]
			ifFalse: 
				[self expression
					ifTrue: 
						[self addComment.
						 stmts addLast: parseNode]
					ifFalse: 
						[self addComment.
						 stmts size = 0 ifTrue: 
							[stmts addLast: 
								(encoder encodeVariable:
									(inner ifTrue: ['nil'] ifFalse: ['self']))]]].
		  returns ifTrue: 
			[self match: #period.
			 (hereType == #rightBracket or: [hereType == #doIt]) ifFalse:
				[^self expected: 'End of block']].
		  returns not and: [self match: #period]] whileTrue].
	(inner and: [argNodes size > 0 and: [stmts isEmpty]]) ifTrue: [
		"A ST-80 empty block should return nil"
		stmts addLast: (encoder encodeVariable: 'nil' sourceRange: nil ifUnknown: [self error: 'Compiler internal error'])].
	theBlockNode
		arguments: argNodes
		statements: stmts
		returns: returns
		from: encoder.
	parseNode := theBlockNode.
	^true
]

{ #category : #'error correction' }
Parser >> substituteSelector: selectorParts wordIntervals: spots [
	"Substitute the correctSelector into the (presuamed interactive) receiver."
	| offset |
	offset := 0.
	selectorParts with: spots do:
		[ :word :interval |
		offset := self substituteWord: word wordInterval: interval offset: offset ]

]

{ #category : #'error correction' }
Parser >> substituteVariable: each atInterval: anInterval [ 
	self 
		substituteWord: each
		wordInterval: anInterval
		offset: 0.
	^encoder encodeVariable: each
]

{ #category : #'error correction' }
Parser >> substituteWord: correctWord wordInterval: spot offset: o [
	"Substitute the correctSelector into the (presuamed interactive) receiver."

	requestor correctFrom: (spot first + o)
					to: (spot last + o)
					with: correctWord.

	requestorOffset := requestorOffset + correctWord size - spot size.
	^ o + correctWord size - spot size
]

{ #category : #'expression types' }
Parser >> temporaries [
	" [ '|' (variable)* '|' ]"
	| vars theActualText |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue: [self interactive
				ifFalse: [tempsMark := 1]
				ifTrue: [tempsMark := requestor selectionInterval first].
			^ #()].
		tempsMark := (prevEnd ifNil: [0]) + 1.
		tempsMark := hereMark	"formerly --> prevMark + prevToken".

		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ #()].
	vars := OrderedCollection new.
	[hereType == #word]
		whileTrue: [vars addLast: (encoder bindTemp: self advance)].
	(self match: #verticalBar) ifTrue: 
		[tempsMark := prevMark.
		^ vars].
	^ self expected: 'Vertical bar'

]

{ #category : #'expression types' }
Parser >> temporariesIn: methodSelector [
	" [ '|' (variable)* '|' ]"
	| vars theActualText |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue: [self interactive
				ifFalse: [tempsMark := 1]
				ifTrue: [tempsMark := requestor selectionInterval first].
			^ #()].
		tempsMark := (prevEnd ifNil: [0]) + 1.
		tempsMark := hereMark	"formerly --> prevMark + prevToken".

		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ #()].
	vars := OrderedCollection new.
	[hereType == #word]
		whileTrue: [vars addLast: (encoder bindTemp: self advance in: methodSelector)].
	(self match: #verticalBar) ifTrue: 
		[tempsMark := prevMark.
		^ vars].
	^ self expected: 'Vertical bar'
]

{ #category : #'expression types' }
Parser >> temporaryBlockVariablesFor: aBlockNode [
	"Scan and answer temporary block variables."

	| variables |
	(self match: #verticalBar) ifFalse:
		"There are't any temporary variables."
		[^#()].

	variables := OrderedCollection new.
	[hereType == #word] whileTrue:
		[variables addLast: (encoder bindBlockTemp: self advance within: aBlockNode)].
	^(self match: #verticalBar)
		ifTrue: [variables]
		ifFalse: [self expected: 'Vertical bar']
]

{ #category : #'expression types' }
Parser >> variable [

	| varName varStart varEnd |
	varStart := self startOfNextToken + requestorOffset.
	varName := self advance.
	varEnd := self endOfLastToken + requestorOffset.
	^ encoder encodeVariable: varName
		sourceRange: (varStart to: varEnd)
		ifUnknown: [self correctVariable: varName interval: (varStart to: varEnd)]
]

{ #category : #private }
Parser >> warns [
	"return whether the parser will ask the user for correction"
	^ true
]
