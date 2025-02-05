"
RBScanner is a stream that returns a sequence of tokens from the string. The tokens know where they came from in the source code and which comments were attached to them.

NOTE: The RBScanner should never be used directly. (If you do so, do it on your own risk!)
The Scanner is private the to Parser, and even the Parser should be used only via the API of the Compiler!

Instance Variables:
	buffer	<PositionableStream>	Accumulates the text for the current token.
	characterType	<ByteSymbol>	The type of the next character. (e.g. #alphabetic, etc.)
	classificationTable	<Array of: Symbol>	Mapping from Character values to their characterType.
	comments	<Collection of: Interval>	Source intervals of scanned comments that must be attached to the next token.
	nextToken <ASTToken>	The ""free"" `next` token, used to store bad comment so no backtracking is needed
	currentCharacter	<Character>	The character currently being processed.
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
	#name : 'OCScanner',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'buffer',
		'tokenStart',
		'currentCharacter',
		'characterType',
		'classificationTable',
		'comments',
		'nextToken',
		'state'
	],
	#classVars : [
		'CascadePatternCharacter',
		'KeywordPatternCharacter',
		'PatternVariableCharacter'
	],
	#classInstVars : [
		'classificationTable'
	],
	#category : 'AST-Core-Parser',
	#package : 'AST-Core',
	#tag : 'Parser'
}

{ #category : 'accessing' }
OCScanner class >> cascadePatternCharacter [
	^ CascadePatternCharacter
]

{ #category : 'accessing' }
OCScanner class >> classificationTable [
	classificationTable
		ifNil: [ self initializeClassificationTable ].
	^ classificationTable
]

{ #category : 'class initialization' }
OCScanner class >> initialize [
	self initializeClassificationTable
]

{ #category : 'class initialization' }
OCScanner class >> initializeChars: characters to: aSymbol [
	characters do: [:c | classificationTable at: c asInteger put: aSymbol]
]

{ #category : 'class initialization' }
OCScanner class >> initializeClassificationTable [
	PatternVariableCharacter := $`.
	KeywordPatternCharacter := $@.
	CascadePatternCharacter := $;.
	classificationTable := Array new: 255.
	self
		initializeChars: (Character allByteCharacters
			select: [ :each | each isLetter ])
		to: #alphabetic.
	self initializeUnderscore.
	self initializeChars: '01234567890' to: #digit.
	self initializeChars: Character specialCharacters to: #binary.
	self initializeChars: '().:;[]{}^' to: #special.
	self
		initializeChars: (Character allByteCharacters
			select: [ :each | each isSeparator ])
		to: #separator
]

{ #category : 'class initialization' }
OCScanner class >> initializeUnderscore [
	self classificationTable
		at: $_ asInteger
		put: #alphabetic
]

{ #category : 'testing' }
OCScanner class >> isSelector: aSymbol [
	^ self basicNew
		on: (ReadStream on: aSymbol asString);
		step;
		isSelector
]

{ #category : 'testing' }
OCScanner class >> isVariable: aString [
	| scanner token |
	scanner := self on: (ReadStream on: aString).
	token := scanner next.
	token isIdentifier ifFalse: [^false].
	(token start = 1 and: [token stop = aString size]) ifFalse: [^false].
	^(aString includes: $.) not
]

{ #category : 'accessing' }
OCScanner class >> keywordPatternCharacter [
	^ KeywordPatternCharacter
]

{ #category : 'instance creation' }
OCScanner class >> new [
	self initializeUnderscore.
	^ super new
]

{ #category : 'instance creation' }
OCScanner class >> on: aStream [ 
	"Create a scanner stream of program tokens on the argument. Users can then use stream API to consume tokens."
	"((RBScanner on: 'Pharo (, { exist ]' readStream) next;  next) value >>> $("

	| scanner |
	scanner := self new on: aStream.
	scanner
		step;
		stripSeparators.
	^scanner
]

{ #category : 'instance creation' }
OCScanner class >> on: aStream errorBlock: aBlock [

	self
		deprecated: 'The scanner no more signals errors or execute error blocks, it just silently produces error tokens'
		transformWith: '`@receiver on: `@arg1 errorBlock: `@arg2' -> '`@receiver on: `@arg1'.
	^ self on: aStream
]

{ #category : 'accessing' }
OCScanner class >> patternVariableCharacter [
	^ PatternVariableCharacter
]

{ #category : 'public api' }
OCScanner class >> scanTokenObjects: aStringOrStream [
	"Return the tokens objects."

	self deprecated: 'Use scanTokens:' 
		on: '18/12/2024'  
		in: 'pharo13'
		transformWith:  '`@receiver scanTokenObjects: `@arg '
			-> '`@receiver scanTokens: `@arg '.
	^ self scanTokens: aStringOrStream
]

{ #category : 'public api' }
OCScanner class >> scanTokens: aStringOrStream [
	"Return the tokens objects."

	| scanner |
	scanner := self on: aStringOrStream readStream.
	^ scanner contents
]

{ #category : 'testing' }
OCScanner >> atEnd [
	^characterType = #eof and: [ nextToken isNil ]
]

{ #category : 'private' }
OCScanner >> classify: aCharacter [

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

{ #category : 'accessing' }
OCScanner >> contents [
	| contentsStream |
	contentsStream := (Array new: 50) writeStream.
	[ self atEnd ]
		whileFalse: [ contentsStream nextPut: self next ].
	^ contentsStream contents
]

{ #category : 'accessing' }
OCScanner >> errorBlock: aBlock [
	self deprecated: 'No-op as errorBlock is removed'
]

{ #category : 'accessing' }
OCScanner >> getComments [
	| oldComments |
	comments isEmpty ifTrue: [^nil].
	oldComments := comments.
	comments := OrderedCollection new: 1.
	^oldComments
]

{ #category : 'testing' }
OCScanner >> isSelector [
	characterType = #binary
		ifTrue: [ [ characterType = #binary ] whileTrue: [ self step ].
			^ characterType = #eof ].
	characterType ~= #alphabetic
		ifTrue: [ ^ false ].
	^ self isSelectorNonBinary
]

{ #category : 'testing' }
OCScanner >> isSelectorNonBinary [
	<script: '(ZnEasy getPng: ''http://www.plantuml.com/plantuml/png/RO-n2i8m48RtUugRXRn0XzBg98wEuX2R4mlNHE8LwTkRL4Xxa2cNx_z2_ki-OgFC4mF8A4nu9QP1GeJRuOx6j7WSlOqBDlXOXy9xVhzipy5Joco-m0vfeoQuP9n2cjFp8PygRJ7_qo259m2ide8REliBBTyGNQa16p9LGUUw1C1_uThk9TBrEl9kdlW7'') inspect'>

	| aState |
	aState := #alphaAlone.
	[ characterType = #alphabetic ]
		whileTrue: [ aState = #colon
				ifTrue: [ aState := #inKeyword ].
			self scanName.
			characterType = #eof
				ifTrue: [ ^ aState = #alphaAlone ].
			currentCharacter = $:
				ifTrue: [ aState := #colon.
					self step ] ].
	characterType = #eof
		ifTrue: [ ^ aState = #colon ].
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

{ #category : 'accessing' }
OCScanner >> next [
	| token |
	"Consume the one remembered (likely a bad comment)"
	nextToken ifNotNil: [ :t |
		nextToken := nil.
		^t ].

	buffer reset.
	tokenStart := stream position.
	token := self scanToken.
	self stripSeparators.
	token comments: self getComments.
	^token
]

{ #category : 'accessing' }
OCScanner >> nextPut: anObject [
	"Provide an error notification that the receiver does not
	implement this message."

	self shouldNotImplement
]

{ #category : 'initialization' }
OCScanner >> on: aStream [
	buffer := (String new: 60) writeStream.
	stream := aStream.
	classificationTable := self class classificationTable.
	comments := OrderedCollection new
]

{ #category : 'private' }
OCScanner >> previousStepPosition [
	"The index of the previous character (that was consumed)"

	^characterType = #eof
		ifTrue: [stream position] "it's the index of the last character"
		ifFalse: [stream position - 1] "-1 to cancel the lookahead"
]

{ #category : 'private - scanning' }
OCScanner >> scanBackFrom: start [
	"Return the verbatim content from the `start` position until the current character.
	 The starting character is included, the current character is not included.
	 The stream status (inclutiong its position) is preserved"

	| len string stop current |
	current := stream position.
	stop := self previousStepPosition.
	len := stop - start + 1.
	stream position: start - 1.
	string := stream next: len.
	stream position: current.
	^ string
]

{ #category : 'private - scanning' }
OCScanner >> scanBinary: aClass [
	"This method isn't supposed to be used in a public context as it accepts any kind of ASTValueToken.
	 The only 2 tokens supposed to be used are ASTLiteralToken and ASTBinarySelectorToken."

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

{ #category : 'private - scanning' }
OCScanner >> scanBinarySelector [
	"Used when binary characters are encountered and not beginning with #"
	^self scanBinary: OCBinarySelectorToken
]

{ #category : 'private - scanning' }
OCScanner >> scanComment [
	| start stop |
	start := stream position.
	buffer reset.
	self step.
	self atEnd
		ifTrue: [ ^ self scanError: 'Unmatched " in comment.' from: start ].
	[ currentCharacter = $" and: [ self step ~= $" ] ]
		whileFalse: [ characterType = #eof
				ifTrue: [ ^ self scanError: 'Unmatched " in comment.' from: start ].
			buffer nextPut: currentCharacter.
			self step ].
	stop := self atEnd
		ifTrue: [ stream position ]
		ifFalse: [ stream position - 1 ].
	^ OCCommentToken value: buffer contents start: start stop: stop
]

{ #category : 'private - scanning' }
OCScanner >> scanError: theCause [

	^ self scanError: theCause from: tokenStart
]

{ #category : 'private - scanning' }
OCScanner >> scanError: theCause from: aPosition [
	"The value of the error token is the verbatim text from aPosition (included) to the current position (not included).
	The error location is index of the current (problematic) character. Or index of the last character + 1 if eof."

	^ OCErrorToken
		  value: (self scanBackFrom: aPosition)
		  start: aPosition
		  cause: theCause
		  location: self previousStepPosition + 1
]

{ #category : 'private - scanning' }
OCScanner >> scanIdentifierOrKeyword [
	"!! Attention !! There are 3 case of Literal Tokens : true, false and nil."
	"This scan accepts keywords only containing a colon."
	"The presence of a colon will mark the creation of a keyword, except when followed with =.
	 Otherwise an Identifier is created."
	"To ensure the pertinence of this scanner, it must be called only if the current character is 	, 	 	 alphabetic."
	| name |
	self scanName.
	(currentCharacter = $: and: [stream peek ~= $=])
		ifTrue: [
			buffer nextPut: currentCharacter.
			self step.
			name := buffer contents.
			^OCKeywordToken value: name start: tokenStart ].
	name := buffer contents.
	(self scanNamedLiteral: name) ifNotNil: [ :token | ^ token ].
	^OCIdentifierToken value: name start: tokenStart
]

{ #category : 'private - scanning' }
OCScanner >> scanLiteral [
	"Do not allow whitespace between # and the literal."

	"Do not allow number literals after the #"

	"This scan accepts keywords as well as binaries and Identifiers."
	self step.
	characterType = #alphabetic
		ifTrue: [ ^ self scanSymbol ].
	characterType = #binary
		ifTrue: [ ^ self scanLiteralBinary ].
	currentCharacter = $'
		ifTrue: [ ^ self scanStringSymbol ].
	(currentCharacter = $( or: [ currentCharacter = $[ ])
		ifTrue: [ ^ self scanLiteralArrayToken ].
	"Accept multiple #."
	currentCharacter = $#
		ifTrue: [ ^ self scanLiteral ].
	^ self scanError: 'Literal expected'
]

{ #category : 'private - scanning' }
OCScanner >> scanLiteralArrayToken [
	"This scan accepts any character but is only meant to be used when a # is followed by ( or [ ."
	| token |
	self step.
	token := OCLiteralArrayToken
				value: (self scanBackFrom: tokenStart)
				start: tokenStart.
	^token
]

{ #category : 'private - scanning' }
OCScanner >> scanLiteralBinary [
	"Used when binary characters are encountered and beginning with #"
	^ (self scanBinary: OCLiteralToken) stop: self previousStepPosition
]

{ #category : 'private - scanning' }
OCScanner >> scanLiteralCharacter [
	"Accepts any character."
	"This scan must only be used after the encounter of a $."
	"'$$' is a correct string to be scanned and the second $ mustn't trigger a new scan."
	| char |
	self step.	"$"
	char := currentCharacter.
	char ifNil:[ ^ self scanError: 'Character expected' ].
	self step.	"char"
	^ self scanLiteralForValue: char
]

{ #category : 'private - scanning' }
OCScanner >> scanLiteralForValue: aValue [

	^ OCLiteralToken
		  value: aValue
		  start: tokenStart
		  stop: self previousStepPosition
]

{ #category : 'private - scanning' }
OCScanner >> scanLiteralString [
	"The current character is the first one after the starting quote"

	| start string |
	[start := stream position.
	string := stream upTo: $'.
	(stream peekBack = $' and: [ stream position > start ]) ifFalse: [
		"the peek logic here is because #upTo: does not tell if given char was found.
		So if the stream did not move of if the last char is not a quote, then we have a problem."
		self step. "to handle eof"
		string := self scanBackFrom: tokenStart. "the whole token, including ' (and #')"
		^ OCErrorToken
			value: string
			start: tokenStart
			cause: 'Unmatched '' in string literal.'
			location: stream position + 1].

	 self step = $'] whileTrue: [
		buffer nextPutAll: string; nextPut: $'.
		string := ''].

	buffer position = 0 ifFalse: [
		buffer nextPutAll: string.
		string := buffer contents].

	"It's possible to use a plain Text for parsing.
	In that case string here could be a Text instance but literal value must be a String"
	^ self scanLiteralForValue: string asString
]

{ #category : 'private - scanning' }
OCScanner >> scanName [
	[characterType = #alphabetic or: [characterType = #digit]] whileTrue:
			[buffer nextPut: currentCharacter.
			self step]
]

{ #category : 'private - scanning' }
OCScanner >> scanNamedLiteral: name [

	name = 'true' ifTrue: [ ^ self scanLiteralForValue: true ].
	name = 'false' ifTrue: [ ^ self scanLiteralForValue: false ].
	name = 'nil' ifTrue: [ ^ self scanLiteralForValue: nil ].
	^ nil
]

{ #category : 'private - scanning' }
OCScanner >> scanNumber [
	| start number stop string gotError |
	start := stream position.
	stream position: start - 1.
	gotError := nil.
	number := NumberParser parse: stream onError: [ :message | gotError := message ].
	stop := stream position.
	self step.
	string := self scanBackFrom: start.
	gotError ifNotNil: [
	^ OCErrorToken
		value: string
		start: tokenStart
		cause: gotError
		location: stop+1
		].
	^OCNumberLiteralToken
		value: number
		start: start
		stop: stop
		source: string
]

{ #category : 'private - scanning' }
OCScanner >> scanSpecialCharacter [
	"Every given character gives an ASTSpecialCharacterToken with only one character as value except for
	 : followed by = which gives an assignement."
	"The case of the assignement is the only one consuming more than 1 character."
	| character |
	currentCharacter = $:
		ifTrue: [
			self step.
			^ currentCharacter = $=
				ifTrue: [
					self step.
					OCAssignmentToken start: tokenStart]
				ifFalse: [ OCSpecialCharacterToken value: $: start: tokenStart ]].

	character := currentCharacter.
	self step.
	^ OCSpecialCharacterToken value: character start: tokenStart
]

{ #category : 'private - scanning' }
OCScanner >> scanStringSymbol [
	"Converting the value of the literal token to a symbol to get rid of a layer of nested comments."
	"Only supposed to be used when encountering an apostrophe."
	| literalToken |
	literalToken := self scanLiteralString.
	literalToken isError ifTrue: [ ^ literalToken. ].
	literalToken value: literalToken value asSymbol.
	^literalToken
]

{ #category : 'private - scanning' }
OCScanner >> scanSymbol [
	"Always return a literal token but only recognises Identifier and keyword strings.
	 Therefor any other string lead to an empty string value."
	self scanSymbolContent.
	^ self scanLiteralForValue: buffer contents asSymbol
]

{ #category : 'private - scanning' }
OCScanner >> scanSymbolContent [
	[ characterType = #alphabetic or: [ currentCharacter = $: ] ] whileTrue: [
		self scanName.
		currentCharacter = $: ifTrue: [
			buffer nextPut: $:.
			self step ] ]
]

{ #category : 'private - scanning' }
OCScanner >> scanSymbolOrNamedLiteral [
	| text |
	self scanSymbolContent.
	text := buffer contents.
	(self scanNamedLiteral: text) ifNotNil: [ :token | ^ token ].
	^ OCLiteralToken
		value: text asSymbol
		start: tokenStart
		stop: self previousStepPosition
		source: (self scanBackFrom: tokenStart)
]

{ #category : 'private - scanning' }
OCScanner >> scanToken [
	"Private method so don't call me. Use next to get the following token. If needed use classSide scanTokens."
	
	"fast-n-ugly. Don't write stuff like this. Has been found to cause cancer in laboratory rats. Basically a
	case statement. Didn't use Dictionary because lookup is pretty slow."

	characterType = #eof ifTrue: [ ^ OCEOFToken start: tokenStart + 1 ]. "The EOF token should occur after the end of input"
	characterType = #alphabetic ifTrue: [
		^ state = #literalArray
			ifTrue: [self scanSymbolOrNamedLiteral]
			ifFalse: [self scanIdentifierOrKeyword]].
	(characterType = #digit
		or: [currentCharacter = $- and: [(self classify: stream peek) = #digit]])
			ifTrue: [^self scanNumber].
	characterType = #binary ifTrue: [
		^ state = #literalArray
			ifTrue: [ self scanLiteralBinary]
			ifFalse: [ self scanBinarySelector]].
	characterType = #special ifTrue: [^self scanSpecialCharacter].
	currentCharacter = $' ifTrue: [^self scanLiteralString].
	currentCharacter = $# ifTrue: [^self scanLiteral].
	currentCharacter = $$ ifTrue: [^self scanLiteralCharacter].
	currentCharacter = $" ifTrue: [^self scanComment]. "Assume a bad comment (as they are usually stripped)"
	^self scanUnknownCharacter
]

{ #category : 'private - scanning' }
OCScanner >> scanUnknownCharacter [
	| errorToken |
	"consume unexpected character"
	self step.
	errorToken := self scanError: 'Unknown character'.
	errorToken location: tokenStart. "Unlike other scanner errors, error location is here before the bad token"
	^ errorToken
]

{ #category : 'accessing' }
OCScanner >> state [

	^ state
]

{ #category : 'accessing' }
OCScanner >> state: anObject [

	state := anObject
]

{ #category : 'private' }
OCScanner >> step [
	stream atEnd
		ifTrue:
			[characterType := #eof.
			^currentCharacter := nil].
	currentCharacter := stream next.
	characterType := self classify: currentCharacter.
	^currentCharacter
]

{ #category : 'private - scanning' }
OCScanner >> stripSeparators [

	[[ characterType = #separator ] whileTrue: [ self step ].
	currentCharacter = $" ] whileTrue: [
		| comment |
		comment := self scanComment.
		comment isError ifTrue: [
			"Remember to use the error as the next token"
			nextToken := comment.
			^ self ].
		comments add: comment ]
]
