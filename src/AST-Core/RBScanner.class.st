"
RBScanner is a stream that returns a sequence of tokens from the string. The tokens know where they came from in the source code and which comments were attached to them.

Instance Variables:
	buffer	<PositionableStream>	Accumulates the text for the current token.
	characterType	<ByteSymbol>	The type of the next character. (e.g. #alphabetic, etc.)
	classificationTable	<Array of: Symbol>	Mapping from Character values to their characterType.
	comments	<Collection of: Interval>	Source intervals of scanned comments that must be attached to the next token.
	nextToken <RBToken>	The ""free"" `next` token, used to store bad comment so no backtracking is needed
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
	#name : #RBScanner,
	#superclass : #Object,
	#instVars : [
		'stream',
		'buffer',
		'tokenStart',
		'currentCharacter',
		'characterType',
		'classificationTable',
		'comments',
		'nextToken'
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
RBScanner class >> cascadePatternCharacter [
	^ CascadePatternCharacter
]

{ #category : #accessing }
RBScanner class >> classificationTable [
	classificationTable
		ifNil: [ self initializeClassificationTable ].
	^ classificationTable
]

{ #category : #'class initialization' }
RBScanner class >> initialize [
	self initializeClassificationTable
]

{ #category : #'class initialization' }
RBScanner class >> initializeChars: characters to: aSymbol [
	characters do: [:c | classificationTable at: c asInteger put: aSymbol]
]

{ #category : #'class initialization' }
RBScanner class >> initializeClassificationTable [
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

{ #category : #'class initialization' }
RBScanner class >> initializeUnderscore [
	self classificationTable
		at: $_ asInteger
		put: #alphabetic
]

{ #category : #testing }
RBScanner class >> isSelector: aSymbol [
	^ self basicNew
		on: (ReadStream on: aSymbol asString);
		step;
		isSelector
]

{ #category : #testing }
RBScanner class >> isVariable: aString [
	| scanner token |
	scanner := self on: (ReadStream on: aString).
	token := scanner next.
	token isIdentifier ifFalse: [^false].
	(token start = 1 and: [token stop = aString size]) ifFalse: [^false].
	^(aString includes: $.) not
]

{ #category : #accessing }
RBScanner class >> keywordPatternCharacter [
	^ KeywordPatternCharacter
]

{ #category : #'instance creation' }
RBScanner class >> new [
	self initializeUnderscore.
	^ super new
]

{ #category : #'instance creation' }
RBScanner class >> on: aStream [ 
	"Create a scanner stream of program tokens on the argument. Users can then use stream API to consume tokens."
	"((RBScanner on: 'Pharo (, { exist ]' readStream) next;  next) value >>> $("

	| scanner |
	scanner := self new on: aStream.
	scanner
		step;
		stripSeparators.
	^scanner
]

{ #category : #'instance creation' }
RBScanner class >> on: aStream errorBlock: aBlock [

	self
		deprecated: 'The scanner no more signals errors or execute error blocks, it just silently produces error tokens'
		transformWith: '`@receiver on: `@arg1 errorBlock: `@arg2' -> '`@receiver on: `@arg1'.
	^ self on: aStream
]

{ #category : #accessing }
RBScanner class >> patternVariableCharacter [
	^ PatternVariableCharacter
]

{ #category : #'public api' }
RBScanner class >> scanTokenObjects: aStringOrStream [
	"Return the tokens objects."

	| scanner |
	scanner := self on: aStringOrStream readStream.
	^ scanner contents
]

{ #category : #testing }
RBScanner >> atEnd [
	^characterType = #eof and: [ nextToken isNil ]
]

{ #category : #private }
RBScanner >> classify: aCharacter [

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
RBScanner >> contents [
	| contentsStream |
	contentsStream := (Array new: 50) writeStream.
	[ self atEnd ]
		whileFalse: [ contentsStream nextPut: self next ].
	^ contentsStream contents
]

{ #category : #accessing }
RBScanner >> errorBlock: aBlock [
	self deprecated: 'No-op as errorBlock is removed'
]

{ #category : #accessing }
RBScanner >> getComments [
	| oldComments |
	comments isEmpty ifTrue: [^nil].
	oldComments := comments.
	comments := OrderedCollection new: 1.
	^oldComments
]

{ #category : #testing }
RBScanner >> isSelector [
	characterType = #binary
		ifTrue: [ [ characterType = #binary ] whileTrue: [ self step ].
			^ characterType = #eof ].
	characterType ~= #alphabetic
		ifTrue: [ ^ false ].
	^ self isSelectorNonBinary
]

{ #category : #testing }
RBScanner >> isSelectorNonBinary [
	<script: '(ZnEasy getPng: ''http://www.plantuml.com/plantuml/png/RO-n2i8m48RtUugRXRn0XzBg98wEuX2R4mlNHE8LwTkRL4Xxa2cNx_z2_ki-OgFC4mF8A4nu9QP1GeJRuOx6j7WSlOqBDlXOXy9xVhzipy5Joco-m0vfeoQuP9n2cjFp8PygRJ7_qo259m2ide8REliBBTyGNQa16p9LGUUw1C1_uThk9TBrEl9kdlW7'') inspect'>

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
RBScanner >> next [
	| token |
	"Consume the one remembered (likely a bad comment)"
	nextToken ifNotNil: [ :t |
		nextToken := nil.
		^t ].

	buffer reset.
	tokenStart := stream position.
	token := characterType = #eof
				ifTrue:
					[RBEOFToken start: tokenStart + 1	"The EOF token should occur after the end of input"]
				ifFalse: [self scanToken].
	self stripSeparators.
	token comments: self getComments.
	^token
]

{ #category : #accessing }
RBScanner >> nextPut: anObject [
	"Provide an error notification that the receiver does not
	implement this message."

	self shouldNotImplement
]

{ #category : #initialization }
RBScanner >> on: aStream [
	buffer := (String new: 60) writeStream.
	stream := aStream.
	classificationTable := self class classificationTable.
	comments := OrderedCollection new
]

{ #category : #private }
RBScanner >> previousStepPosition [
	^characterType = #eof
		ifTrue: [stream position]
		ifFalse: [stream position - 1]
]

{ #category : #'private - scanning' }
RBScanner >> scanBackFrom: start [
	"Return the verbatim content from the `start` position until the current position.
	 The starting and current character are both included.
	 The stream status (inclutiong its position) is preserved"

	| len string stop |
	stop := stream position.
	len := stop - start + 1.
	stream position: start - 1.
	string := stream next: len.
	stream position: stop.
	^ string
]

{ #category : #'private - scanning' }
RBScanner >> scanBinary: aClass [
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
RBScanner >> scanBinarySelector [
	"Used when binary characters are encountered and not beginning with #"
	^self scanBinary: RBBinarySelectorToken
]

{ #category : #'private - scanning' }
RBScanner >> scanComment [
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
	^ RBCommentToken value: buffer contents start: start stop: stop
]

{ #category : #'private - scanning' }
RBScanner >> scanError: theCause [
	"An error token accepts every character given, recognised or not."
	"The value of an error token is one character maximum : the current character."
	| location |
	"error location is the next not parseable character. Or current stream position + 1 if
	an expected character is missing."
	location := stream position.
	currentCharacter
		ifNotNil: [ :char | buffer nextPut: char ]
		ifNil: [ location := stream position + 1 ].
	^ RBErrorToken
		value: buffer contents asString
		start: tokenStart
		cause: theCause
		location: location
]

{ #category : #'private - scanning' }
RBScanner >> scanError: theCause from: aPosition [
	"An error token accepts every character given, recognised or not."
	"The value of the error token is the verbatim text from aPosition to the current position (both included)."
	| location |
	"error location is the next not parseable character. Or current stream position + 1 if
	an expected character is missing."
	location := stream position.
	currentCharacter
		ifNotNil: [ :char | buffer nextPut: char ]
		ifNil: [ location := stream position + 1 ].
	^ RBErrorToken
		value: (self scanBackFrom: aPosition)
		start: aPosition
		cause: theCause
		location: location
]

{ #category : #'private - scanning' }
RBScanner >> scanIdentifierOrKeyword [
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
			[^RBLiteralToken
				value: true
				start: tokenStart
				stop: self previousStepPosition].
	name = 'false'
		ifTrue:
			[^RBLiteralToken
				value: false
				start: tokenStart
				stop: self previousStepPosition].
	name = 'nil'
		ifTrue:
			[^RBLiteralToken
				value: nil
				start: tokenStart
				stop: self previousStepPosition].
	^RBIdentifierToken value: name start: tokenStart
]

{ #category : #'private - scanning' }
RBScanner >> scanKeyword [
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
		ifTrue: [RBKeywordToken value: name start: tokenStart]
		ifFalse:
			[RBMultiKeywordLiteralToken
				value: name asSymbol
				start: tokenStart
				stop: tokenStart + name size - 1]
]

{ #category : #'private - scanning' }
RBScanner >> scanLiteral [
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
	^ (self scanError: 'Literal expected') value: '#'
]

{ #category : #'private - scanning' }
RBScanner >> scanLiteralArrayToken [
	"This scan accepts any character but is only meant to be used when a # is followed by ( or [ ."
	| token |
	token := RBLiteralArrayToken
				value: (String with: $# with: currentCharacter)
				start: tokenStart.
	self step.
	^token
]

{ #category : #'private - scanning' }
RBScanner >> scanLiteralBinary [
	"Used when binary characters are encountered and beginning with #"
	^self scanBinary: RBLiteralToken
]

{ #category : #'private - scanning' }
RBScanner >> scanLiteralCharacter [
	"Accepts any character."
	"This scan must only be used after the encounter of a $."
	"'$$' is a correct string to be scanned and the second $ mustn't trigger a new scan."
	| token |
	self step.	"$"
	currentCharacter ifNil:[ ^ (self scanError:'Character expected') value: '$'].
	token := RBLiteralToken
				value: currentCharacter
				start: tokenStart
				stop: stream position.
	self step.	"char"
	^token
]

{ #category : #'private - scanning' }
RBScanner >> scanLiteralString [
	"The current character is the first one after the starting quote"

	| start string |
	[start := stream position.
	string := stream upTo: $'.
	(stream peekBack = $' and: [ stream position > start ]) ifFalse: [
		"the peek logic here is because #upTo: does not tell if given char was found.
		So if the stream did not move of if the last char is not a quote, then we have a problem."
		string := self scanBackFrom: tokenStart. "the whole token, including ' (and #')"
		self step. "to handle eof"
		^ RBErrorToken
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
	^RBLiteralToken
		value: string asString
		start: tokenStart
		stop: self previousStepPosition
]

{ #category : #'private - scanning' }
RBScanner >> scanName [
	[characterType = #alphabetic or: [characterType = #digit]] whileTrue:
			[buffer nextPut: currentCharacter.
			self step]
]

{ #category : #'private - scanning' }
RBScanner >> scanNumber [
	| start number stop string gotError |
	start := stream position.
	stream position: start - 1.
	gotError := nil.
	number := NumberParser parse: stream onError: [ :message | gotError := message ].
	stop := stream position.
	string := self scanBackFrom: start.
	self step.
	gotError ifNotNil: [
	^ RBErrorToken
		value: string
		start: tokenStart
		cause: gotError
		location: stop+1
		].
	^RBNumberLiteralToken
		value: number
		start: start
		stop: stop
		source: string
]

{ #category : #'private - scanning' }
RBScanner >> scanSpecialCharacter [
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
					RBAssignmentToken start: tokenStart]
				ifFalse: [ RBSpecialCharacterToken value: $: start: tokenStart ]].

	character := currentCharacter.
	self step.
	^ RBSpecialCharacterToken value: character start: tokenStart
]

{ #category : #'private - scanning' }
RBScanner >> scanStringSymbol [
	"Converting the value of the literal token to a symbol to get rid of a layer of nested comments."
	"Only supposed to be used when encountering an apostrophe."
	| literalToken |
	literalToken := self scanLiteralString.
	literalToken isError ifTrue: [ ^ literalToken. ].
	literalToken value: literalToken value asSymbol.
	^literalToken
]

{ #category : #'private - scanning' }
RBScanner >> scanSymbol [
	"Always return a literal token but only recognises Identifier and keyword strings.
	 Therefor any other string lead to an empty string value."
	[ characterType = #alphabetic or: [ currentCharacter = $: ] ] whileTrue: [
		self scanName.
		currentCharacter = $: ifTrue: [
			buffer nextPut: $:.
			self step ] ].
	^ RBLiteralToken
		value: buffer contents asSymbol
		start: tokenStart
		stop: self previousStepPosition
]

{ #category : #accessing }
RBScanner >> scanToken [
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
	currentCharacter = $" ifTrue: [^self scanComment]. "Assume a bad comment (as they are usually stripped)"
	^self scanUnknownCharacter
]

{ #category : #'private - scanning' }
RBScanner >> scanUnknownCharacter [
	| errorToken |
	errorToken := self scanError: 'Unknown character'.
	"advance"
	self step.
	^ errorToken
]

{ #category : #private }
RBScanner >> step [
	stream atEnd
		ifTrue:
			[characterType := #eof.
			^currentCharacter := nil].
	currentCharacter := stream next.
	characterType := self classify: currentCharacter.
	^currentCharacter
]

{ #category : #'private - scanning' }
RBScanner >> stripSeparators [

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
