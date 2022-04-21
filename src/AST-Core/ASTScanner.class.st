"
ASTScanner is a stream that returns a sequence of tokens from the string. The tokens know where they came from in the source code and which comments were attached to them.

Instance Variables:
	buffer	<PositionableStream>	Accumulates the text for the current token.
	characterType	<ByteSymbol>	The type of the next character. (e.g. #alphabetic, etc.)
	classificationTable	<Array of: Symbol>	Mapping from Character values to their characterType.
	comments	<Collection of: Interval>	Source intervals of scanned comments that must be attached to the next token.
	currentCharacter	<Character>	The character currently being processed.
	errorBlock	<BlockClosure>	The block to execute on lexical errors.
	extendedLiterals	<Boolean>	True if IBM-type literals are allowed. In VW, this is false.
	nameSpaceCharacter	<Character>	The character used to separate namespaces.
	numberType	<ByteSymbol>	The method to perform: to scan a number. 
	separatorsInLiterals	<Boolean>	True if separators are allowed within literals.
	stream	<PositionableStream>	Contains the text to be scanned.
	tokenStart	<Integer>	The source position of the beginning of the current token

Class Instance Variables:
	classificationTable	<Array>		the default classification table for all characters

Shared Variables:
	PatternVariableCharacter	<Character>	the character that starts a pattern node
"
Class {
	#name : #ASTScanner,
	#superclass : #Object,
	#instVars : [
		'stream',
		'buffer',
		'tokenStart',
		'currentCharacter',
		'characterType',
		'classificationTable',
		'comments',
		'errorBlock'
	],
	#classVars : [
		'CascadePatternCharacter',
		'KeywordPatternCharacter',
		'PatternVariableCharacter'
	],
	#classInstVars : [
		'classificationTable'
	],
	#category : #'AST-Core-Parser'
}

{ #category : #accessing }
ASTScanner class >> cascadePatternCharacter [
	^ CascadePatternCharacter
]

{ #category : #accessing }
ASTScanner class >> classificationTable [
	classificationTable 
		ifNil: [ self initializeClassificationTable ].
	^ classificationTable
]

{ #category : #'class initialization' }
ASTScanner class >> initialize [
	self initializeClassificationTable
]

{ #category : #'class initialization' }
ASTScanner class >> initializeChars: characters to: aSymbol [ 
	characters do: [:c | classificationTable at: c asInteger put: aSymbol]
]

{ #category : #'class initialization' }
ASTScanner class >> initializeClassificationTable [
	PatternVariableCharacter := $`.
	KeywordPatternCharacter := $@.
	CascadePatternCharacter := $;.
	classificationTable := Array new: 255.
	self 
		initializeChars: (Character allCharacters
			select: [ :each | each isLetter ])
		to: #alphabetic.
	self initializeUnderscore.
	self initializeChars: '01234567890' to: #digit.
	self initializeChars: Character specialCharacters to: #binary.
	self initializeChars: '().:;[]{}^' to: #special.
	self
		initializeChars: (Character allCharacters
			select: [ :each | each isSeparator ])
		to: #separator
]

{ #category : #'class initialization' }
ASTScanner class >> initializeUnderscore [
	self classificationTable
		at: $_ asInteger
		put: #alphabetic
]

{ #category : #testing }
ASTScanner class >> isSelector: aSymbol [
	^ self basicNew
		on: (ReadStream on: aSymbol asString); 
		step;
		isSelector
]

{ #category : #testing }
ASTScanner class >> isVariable: aString [ 
	| scanner token |
	scanner := self on: (ReadStream on: aString).
	token := scanner next.
	token isIdentifier ifFalse: [^false].
	(token start = 1 and: [token stop = aString size]) ifFalse: [^false].
	^(aString includes: $.) not
]

{ #category : #accessing }
ASTScanner class >> keywordPatternCharacter [
	^ KeywordPatternCharacter
]

{ #category : #'instance creation' }
ASTScanner class >> new [
	self initializeUnderscore.
	^ super new
]

{ #category : #'instance creation' }
ASTScanner class >> on: aStream [ 
	"Create a scanner stream of program tokens on the argument. Users can then use stream API to consume tokens. Reader may consider scanTokens: as a better way to use the class."
	"((RBScanner on: 'Pharo (, { exist ]' readStream) next;  next) value >>> $("

	| str |
	str := self new on: aStream.
	str
		step;
		stripSeparators.
	^str
]

{ #category : #'instance creation' }
ASTScanner class >> on: aStream errorBlock: aBlock [ 
	| str |
	str := self new on: aStream.
	str
		errorBlock: aBlock;
		step;
		stripSeparators.
	^str
]

{ #category : #accessing }
ASTScanner class >> patternVariableCharacter [
	^ PatternVariableCharacter
]

{ #category : #'public api' }
ASTScanner class >> scanTokenObjects: aStringOrStream [
	"Return the tokens objects."
	
	| scanner |
	scanner := self on: aStringOrStream readStream.
	^ scanner contents
]

{ #category : #'public api' }
ASTScanner class >> scanTokens: aStringOrStream [
	"Return the tokens (and not the scanner token objects)."

	^ (self scanTokenObjects: aStringOrStream) collect: [ :t | t value ]
]

{ #category : #testing }
ASTScanner >> atEnd [
	^characterType = #eof
]

{ #category : #private }
ASTScanner >> classify: aCharacter [

	| index |

	aCharacter ifNil: [ ^ nil ].
	index := aCharacter asInteger.
	index == 0
		ifTrue: [ ^ #separator ].
	index > 255
		ifTrue: [ ^ aCharacter isLetter
				ifTrue: [ #alphabetic ]
				ifFalse: [ aCharacter isSeparator
						ifTrue: [ #separator ]
						ifFalse: [ nil ]
					]
			].
	^ classificationTable at: index
]

{ #category : #accessing }
ASTScanner >> contents [
	| contentsStream |
	contentsStream := (Array new: 50) writeStream.
	[ self atEnd ]
		whileFalse: [ contentsStream nextPut: self next ].
	^ contentsStream contents
]

{ #category : #'error handling' }
ASTScanner >> errorBlock [
	^errorBlock ifNil: [[:message :position | ]] ifNotNil: [errorBlock]
]

{ #category : #accessing }
ASTScanner >> errorBlock: aBlock [ 
	errorBlock := aBlock
]

{ #category : #'error handling' }
ASTScanner >> errorPosition [
	^stream position
]

{ #category : #accessing }
ASTScanner >> getComments [
	| oldComments |
	comments isEmpty ifTrue: [^nil].
	oldComments := comments.
	comments := OrderedCollection new: 1.
	^oldComments
]

{ #category : #testing }
ASTScanner >> isSelector [
	characterType = #binary
		ifTrue: [ [ characterType = #binary ] whileTrue: [ self step ].
			^ characterType = #eof ].
	characterType ~= #alphabetic
		ifTrue: [ ^ false ].
	^ self isSelectorNonBinary
]

{ #category : #testing }
ASTScanner >> isSelectorNonBinary [
	"inspect the next line and view as morph"

	"ZnEasy getPng: 'http://www.plantuml.com/plantuml/png/RO-n2i8m48RtUugRXRn0XzBg98wEuX2R4mlNHE8LwTkRL4Xxa2cNx_z2_ki-OgFC4mF8A4nu9QP1GeJRuOx6j7WSlOqBDlXOXy9xVhzipy5Joco-m0vfeoQuP9n2cjFp8PygRJ7_qo259m2ide8REliBBTyGNQa16p9LGUUw1C1_uThk9TBrEl9kdlW7'"

	| state |
	state := #alphaAlone.
	[ characterType = #alphabetic ]
		whileTrue: [ state = #colon
				ifTrue: [ state := #inKeyword ].
			self scanName.
			characterType = #eof
				ifTrue: [ ^ state = #alphaAlone ].
			currentCharacter = $:
				ifTrue: [ state := #colon.
					self step ] ].
	characterType = #eof
		ifTrue: [ ^ state = #colon ].
	^ false

	"Diagram by:
@startuml

title Selector
[*] --> Binary : binary
[*] --> AlphaAlone : alpha
AlphaAlone --> AlphaAlone: alpha
AlphaAlone -d-> Colon : colon
AlphaAlone -> [*] : eof


Colon -> [*] : eof
Colon -> InKeyword : alpha

InKeyword --> InKeyword : alpha
InKeyword --> Colon : colon


Binary --> Binary : binary
Binary --> [*] : eof

@enduml"
]

{ #category : #accessing }
ASTScanner >> next [
	| token |
	buffer reset.
	tokenStart := stream position.
	token := characterType = #eof 
				ifTrue: 
					[ASTEOFToken start: tokenStart + 1	"The EOF token should occur after the end of input"]
				ifFalse: [self scanToken].
	self stripSeparators.
	token comments: self getComments.
	^token
]

{ #category : #accessing }
ASTScanner >> nextPut: anObject [ 
	"Provide an error notification that the receiver does not
	implement this message."

	self shouldNotImplement
]

{ #category : #initialization }
ASTScanner >> on: aStream [ 
	buffer := (String new: 60) writeStream.
	stream := aStream.
	classificationTable := self class classificationTable.
	comments := OrderedCollection new
]

{ #category : #'error handling' }
ASTScanner >> parseErrorNode: aMessageString [
	| sourceString |
	sourceString := stream contents copyFrom: self errorPosition to: stream contents size.
	^ ASTParseErrorNode
		errorMessage: aMessageString value: sourceString at: self errorPosition
]

{ #category : #private }
ASTScanner >> previousStepPosition [
	^characterType = #eof 
		ifTrue: [stream position]
		ifFalse: [stream position - 1]
]

{ #category : #'private - scanning' }
ASTScanner >> scanBinary: aClass [ 
	"This method isn't supposed to be used in a public context as it accepts any kind of RBValueToken.
	 The only 2 tokens supposed to be used are RBLiteralToken and RBBinarySelectorToken."
	
	"This method trims separators but doesn't add comments to the token created."
	
	"Puts any succession of selectors in the token, even meaningless ones."
	| val |
	buffer nextPut: currentCharacter.
	self step.
	[ characterType = #binary ] whileTrue: 
		[ buffer nextPut: currentCharacter.
		self step ].
	val := buffer contents.
	val := val asSymbol.
	^aClass value: val start: tokenStart
]

{ #category : #'private - scanning' }
ASTScanner >> scanBinarySelector [
	"Used when binary characters are encountered and not beginning with #"
	^self scanBinary: ASTBinarySelectorToken
]

{ #category : #'private - scanning' }
ASTScanner >> scanComment [
	| start stop |
	start := stream position.
	buffer reset.
	self step.
	self atEnd
		ifTrue: [ ^ self scannerError: 'Unmatched " in comment.' ].
	[ currentCharacter = $" and: [ buffer nextPut: currentCharacter. self step ~= $" ] ]
		whileFalse: [ characterType = #eof
				ifTrue: [ ^ self scannerError: 'Unmatched " in comment.' ].
			buffer nextPut: currentCharacter.
			self step ].
	stop := self atEnd
		ifTrue: [ stream position ]
		ifFalse: [ stream position - 1 ].
	comments add: (ASTCommentToken value: (buffer contents copyFrom: 1 to: buffer contents size -1) start: start stop: stop).
]

{ #category : #'private - scanning' }
ASTScanner >> scanError: theCause [
	"An error token accepts every character given, recognised or not."
	"The value of an error token is one character maximum : the current character."
	| location |
	"error location is the next not parseable character. Or current stream position + 1 if 
	an expected character is missing."
	location := stream position.
	currentCharacter
		ifNotNil: [ :char | buffer nextPut: char ]
		ifNil: [ location := stream position + 1 ].
	^ ASTErrorToken
		value: buffer contents asString
		start: tokenStart
		cause: theCause
		location: location
]

{ #category : #'private - scanning' }
ASTScanner >> scanIdentifierOrKeyword [
	"!! Attention !! There are 3 case of Literal Tokens : true, false and nil."
	"This scan accepts keywords only containing a colon."
	"The presence of a colon will mark the creation of a keyword, except when followed with =.
	 Otherwise an Identifier is created."
	"To ensure the pertinence of this scanner, it must be called only if the current character is 	, 	 	 alphabetic."
	| name |
	self scanName.
	(currentCharacter = $: and: [stream peek ~= $=]) 
		ifTrue: [^self scanKeyword].
	name := buffer contents.
	name = 'true' 
		ifTrue: 
			[^ASTLiteralToken 
				value: true
				start: tokenStart
				stop: self previousStepPosition].
	name = 'false' 
		ifTrue: 
			[^ASTLiteralToken 
				value: false
				start: tokenStart
				stop: self previousStepPosition].
	name = 'nil' 
		ifTrue: 
			[^ASTLiteralToken 
				value: nil
				start: tokenStart
				stop: self previousStepPosition].
	^ASTIdentifierToken value: name start: tokenStart
]

{ #category : #'private - scanning' }
ASTScanner >> scanKeyword [
	"This scan can actually give odd keywords like a single colon.
	 : is a keyword but it is an impossible one to reach with a call to next.
	 However, a call to next can give an RBMultiKeywordLiteralToken even if the string scanned is like
	 the following : 'keyword:::::::'. Here, each colon is considered a keyword."
	"Of course, a keyword must finish with a colon. That's why arguments are not taken into account."
	| outputPosition inputPosition name |
	[currentCharacter = $:] whileTrue: 
			[buffer nextPut: currentCharacter.
			outputPosition := buffer position.
			inputPosition := stream position.
			self step.	":"
			[characterType = #alphabetic] whileTrue: [self scanName]].
	buffer position: outputPosition.
	stream position: inputPosition.
	self step.
	name := buffer contents.
	^(name occurrencesOf: $:) == 1 
		ifTrue: [ASTKeywordToken value: name start: tokenStart]
		ifFalse: 
			[ASTMultiKeywordLiteralToken 
				value: name asSymbol
				start: tokenStart
				stop: tokenStart + name size - 1]
]

{ #category : #'private - scanning' }
ASTScanner >> scanLiteral [
	"Do not allow whitespace between # and the literal."

	"Do not allow number literals after the #"

	"This scan accepts keywords as well as binaries and Identifiers."
	self step.
	characterType = #alphabetic
		ifTrue: [ ^ self scanSymbol ].
	characterType = #binary
		ifTrue:
			[ ^ (self scanLiteralBinary)
				stop: self previousStepPosition ].
	currentCharacter = $'
		ifTrue: [ ^ self scanStringSymbol ].
	(currentCharacter = $( or: [ currentCharacter = $[ ])
		ifTrue: [ ^ self scanLiteralArrayToken ].
	"Accept multiple #."
	currentCharacter = $#
		ifTrue: [ ^ self scanLiteral ].
	^ (self scanError: 'Expecting a literal type') value: '#'.
]

{ #category : #'private - scanning' }
ASTScanner >> scanLiteralArrayToken [
	"This scan accepts any character but is only meant to be used when a # is followed by ( or [ ."
	| token |
	token := ASTLiteralArrayToken 
				value: (String with: $# with: currentCharacter)
				start: tokenStart.
	self step.
	^token
]

{ #category : #'private - scanning' }
ASTScanner >> scanLiteralBinary [
	"Used when binary characters are encountered and beginning with #"
	^self scanBinary: ASTLiteralToken
]

{ #category : #'private - scanning' }
ASTScanner >> scanLiteralCharacter [
	"Accepts any character."
	"This scan must only be used after the encounter of a $."
	"'$$' is a correct string to be scanned and the second $ mustn't trigger a new scan."
	| token |
	self step.	"$"
	currentCharacter ifNil:[ ^ (self scanError:'A Character was expected') value: '$'].
	token := ASTLiteralToken 
				value: currentCharacter
				start: tokenStart
				stop: stream position.
	self step.	"char"
	^token
]

{ #category : #'private - scanning' }
ASTScanner >> scanLiteralString [
	"String can contain anything, even faulty code."
	| string |
	[stream atEnd ifTrue: [
		self step. "to handle eof"
		^ self scanError: 'Unmatched '' in string literal.'].
	string := stream upTo: $'. 
	stream peekBack = $' ifFalse: [ 
		"the peek logic here is because #upTo: does not tell if given char was found"
		buffer nextPutAll: string.
		self step. "to handle eof"
		^ self scanError: 'Unmatched '' in string literal.'].
	
	self step = $'] whileTrue: [ 
		buffer nextPutAll: string; nextPut: $'.
		string := ''].
	
	buffer position = 0 ifFalse: [ 		
		buffer nextPutAll: string.
		string := buffer contents].	
	
	"It's possible to use a plain Text for parsing.
	In that case string here could be a Text instance but literal value must be a String"	
	^ASTLiteralToken 
		value: string asString 
		start: tokenStart 
		stop: self previousStepPosition
]

{ #category : #'private - scanning' }
ASTScanner >> scanName [
	[characterType = #alphabetic or: [characterType = #digit]] whileTrue: 
			[buffer nextPut: currentCharacter.
			self step]
]

{ #category : #'private - scanning' }
ASTScanner >> scanNumber [
	| start number stop string |
	start := stream position.
	stream position: start - 1.
	number := [NumberParser parse: stream] on: Error do: [:err | self scannerError: err messageText].
	stop := stream position.
	stream position: start - 1.
	string := stream next: stop - start + 1.
	stream position: stop.
	self step.
	^ASTNumberLiteralToken 
		value: number
		start: start
		stop: stop
		source: string
]

{ #category : #'private - scanning' }
ASTScanner >> scanSpecialCharacter [
	"Every given character gives a RBSpecialCharacterToken with only one character as value except for
	 : followed by = which gives an assignement."
	"The case of the assignement is the only one consuming more than 1 character."
	| character |
	currentCharacter = $: 
		ifTrue: [
			self step.
			^ currentCharacter = $= 
				ifTrue: [
					self step.
					ASTAssignmentToken start: tokenStart]
				ifFalse: [ ASTSpecialCharacterToken value: $: start: tokenStart ]].
	
	character := currentCharacter.
	self step.
	^ ASTSpecialCharacterToken value: character start: tokenStart
]

{ #category : #'private - scanning' }
ASTScanner >> scanStringSymbol [
	"Converting the value of the literal token to a symbol to get rid of a layer of nested comments."
	"Only supposed to be used when encountering an apostrophe."
	| literalToken |
	literalToken := self scanLiteralString.
	literalToken value: literalToken value asSymbol.
	^literalToken
]

{ #category : #'private - scanning' }
ASTScanner >> scanSymbol [
	"Always return a literal token but only recognises Identifier and keyword strings.
	 Therefor any other string lead to an empty string value."
	[ characterType = #alphabetic or: [ currentCharacter = $: ] ] whileTrue: [
		self scanName.
		currentCharacter = $: ifTrue: [
			buffer nextPut: $:.
			self step ] ].
	^ ASTLiteralToken 
		value: buffer contents asSymbol
		start: tokenStart
		stop: self previousStepPosition
]

{ #category : #accessing }
ASTScanner >> scanToken [
	"fast-n-ugly. Don't write stuff like this. Has been found to cause cancer in laboratory rats. Basically a 
	case statement. Didn't use Dictionary because lookup is pretty slow."

	characterType = #alphabetic ifTrue: [^self scanIdentifierOrKeyword].
	(characterType = #digit 
		or: [currentCharacter = $- and: [(self classify: stream peek) = #digit]]) 
			ifTrue: [^self scanNumber].
	characterType = #binary ifTrue: [^self scanBinarySelector].
	characterType = #special ifTrue: [^self scanSpecialCharacter].
	currentCharacter = $' ifTrue: [^self scanLiteralString].
	currentCharacter = $# ifTrue: [^self scanLiteral].
	currentCharacter = $$ ifTrue: [^self scanLiteralCharacter].
	^self scanUnknownCharacter
]

{ #category : #'private - scanning' }
ASTScanner >> scanUnknownCharacter [
	| errorToken |
	errorToken := self scanError: 'Unknown character'.
	"advance"
	self step.
	^ errorToken
]

{ #category : #'error handling' }
ASTScanner >> scannerError: aString [
	(self errorBlock cull: aString cull: self errorPosition cull: self) ifNil: [ 
		^ SyntaxErrorNotification
					inClass: Object
					withCode: stream contents
					doitFlag: false
					errorMessage: aString
					location: stream position + 1
	]
]

{ #category : #private }
ASTScanner >> step [
	stream atEnd 
		ifTrue: 
			[characterType := #eof.
			^currentCharacter := nil].
	currentCharacter := stream next.
	characterType := self classify: currentCharacter.
	^currentCharacter
]

{ #category : #'private - scanning' }
ASTScanner >> stripSeparators [
	
	[[characterType = #separator] whileTrue: [self step].
	currentCharacter = $"] 
			whileTrue: [self scanComment]
]
