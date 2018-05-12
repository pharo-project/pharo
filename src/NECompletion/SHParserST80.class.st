"
I am a Smalltalk method / expression parser supporting syntax highligthing. 
"
Class {
	#name : #SHParserST80,
	#superclass : #Object,
	#instVars : [
		'classOrMetaClass',
		'source',
		'workspace',
		'arguments',
		'sourcePosition',
		'currentToken',
		'currentTokenFirst',
		'temporaries',
		'instanceVariables',
		'errorBlock',
		'currentTokenSourcePosition',
		'blockDepth',
		'bracketDepth',
		'ranges',
		'environment',
		'isMethod'
	],
	#category : #'NECompletion-Parser'
}

{ #category : #accessing }
SHParserST80 >> classOrMetaClass: aClass [ 
	classOrMetaClass := aClass
]

{ #category : #scan }
SHParserST80 >> currentChar [
	^ source
		at: sourcePosition
		ifAbsent: []
]

{ #category : #'parse support' }
SHParserST80 >> enterBlock [
	blockDepth := blockDepth + 1.
	bracketDepth := bracketDepth + 1
]

{ #category : #accessing }
SHParserST80 >> environment: anObject [ 
	environment := anObject
]

{ #category : #'error handling' }
SHParserST80 >> error [
	self
		rangeType: #excessCode
		start: (ranges isEmpty
				ifTrue: [1]
				ifFalse: [ranges last end + 1])
		end: source size.
	errorBlock value
]

{ #category : #'error handling' }
SHParserST80 >> failUnless: aBoolean [ 
	aBoolean
		ifFalse: [self error]
]

{ #category : #'error handling' }
SHParserST80 >> failWhen: aBoolean [ 
	aBoolean
		ifTrue: [self error]
]

{ #category : #initialization }
SHParserST80 >> initialize [
	environment := Smalltalk
]

{ #category : #'parse support' }
SHParserST80 >> initializeInstanceVariables [
	instanceVariables := classOrMetaClass notNil
				ifTrue: [classOrMetaClass allInstVarNames asArray]
				ifFalse: [Set new]
]

{ #category : #'token testing' }
SHParserST80 >> isAnsiAssignment [
	^ currentToken = ':='
]

{ #category : #'token testing' }
SHParserST80 >> isAssignment [
	^ currentToken = ':='
		or: [currentToken = '_']
]

{ #category : #'character testing' }
SHParserST80 >> isBigDigit: aCharacter base: anInteger [ 
	| digitValue |
	digitValue := aCharacter digitValue.
	^ digitValue >= 0
		and: [digitValue < anInteger]
]

{ #category : #'token testing' }
SHParserST80 >> isBinary [
	| char |
	(currentToken isNil
			or: [self isName
					or: [self isKeyword]])
		ifTrue: [^ false].
	1
		to: currentToken size
		do: [:i | 
			char := currentToken at: i.
			(self isSelectorCharacter: char)
				ifFalse: [^ false]].
	^ true
]

{ #category : #'identifier testing' }
SHParserST80 >> isBlockArgName: aString [ 
	| temp arg |
	blockDepth
		to: 1
		by: -1
		do: [:level | 
			arg := (arguments
						at: level
						ifAbsent: [#()])
						includes: aString.
			arg
				ifTrue: [^ true].
			temp := (temporaries
						at: level
						ifAbsent: [#()])
						includes: aString.
			temp
				ifTrue: [^ false]].
	^ false
]

{ #category : #'identifier testing' }
SHParserST80 >> isBlockTempName: aString [ 
	| temp arg |
	blockDepth
		to: 1
		by: -1
		do: [:level | 
			arg := (arguments
						at: level
						ifAbsent: [#()])
						includes: aString.
			arg
				ifTrue: [^ false].
			temp := (temporaries
						at: level
						ifAbsent: [#()])
						includes: aString.
			temp
				ifTrue: [^ true]].
	^ false
]

{ #category : #'identifier testing' }
SHParserST80 >> isIncompleteBlockArgName: aString [ 
	| arg |
	blockDepth
		to: 1
		by: -1
		do: [:level | 
			arg := (arguments
						at: level
						ifAbsent: [#()])
						anySatisfy: [:each | each beginsWith: aString].
			arg
				ifTrue: [^ true]].
	^ false
]

{ #category : #'identifier testing' }
SHParserST80 >> isIncompleteBlockTempName: aString [ 
	| temp |
	blockDepth
		to: 1
		by: -1
		do: [:level | 
			temp := (temporaries
						at: level
						ifAbsent: [#()])
						anySatisfy: [:each | each beginsWith: aString].
			temp
				ifTrue: [^ true]].
	^ false
]

{ #category : #'identifier testing' }
SHParserST80 >> isIncompleteMethodArgName: aString [ 
	^ (arguments
		at: 0
		ifAbsent: [#()])
		anySatisfy: [:each | each beginsWith: aString]
]

{ #category : #'identifier testing' }
SHParserST80 >> isIncompleteMethodTempName: aString [ 
	^ (temporaries
		at: 0
		ifAbsent: [#()])
		anySatisfy: [:each | each beginsWith: aString]
]

{ #category : #'token testing' }
SHParserST80 >> isKeyword [
	^ (currentTokenFirst isLetter
			or: [currentTokenFirst == $_])
		and: [currentToken last == $:]
]

{ #category : #accessing }
SHParserST80 >> isMethod [

	^ isMethod ifNil: [ classOrMetaClass notNil ].
]

{ #category : #accessing }
SHParserST80 >> isMethod: aBoolean [

	isMethod := aBoolean.
]

{ #category : #'identifier testing' }
SHParserST80 >> isMethodArgName: aString [ 
	^ (arguments
		at: 0
		ifAbsent: [#()])
		includes: aString
]

{ #category : #'identifier testing' }
SHParserST80 >> isMethodTempName: aString [ 
	((arguments
			at: 0
			ifAbsent: [#()])
			includes: aString)
		ifTrue: [^ false].
	^ (temporaries
		at: 0
		ifAbsent: [#()])
		includes: aString
]

{ #category : #'token testing' }
SHParserST80 >> isName [
	^ (currentTokenFirst isLetter
			or: [currentTokenFirst == $_])
		and: [currentToken last isAlphaNumeric
				or: [currentToken last == $_]]
]

{ #category : #'character testing' }
SHParserST80 >> isSelectorCharacter: aCharacter [ 
	aCharacter isAlphaNumeric
		ifTrue: [^ false].
	aCharacter isSeparator
		ifTrue: [^ false].
	('"#$'':().;[]{}^_' includes: aCharacter)
		ifTrue: [^ false].
	aCharacter asciiValue = 30
		ifTrue: [^ false].
	aCharacter asciiValue = 0
		ifTrue: [^ false].
	^ true
]

{ #category : #'token testing' }
SHParserST80 >> isTokenExternalFunctionCallingConvention [
	| descriptorClass |
	descriptorClass := Smalltalk
				at: #ExternalFunction
				ifAbsent: [].
	descriptorClass
		ifNil: [^ false].
	^ (descriptorClass callingConventionFor: currentToken) notNil
]

{ #category : #'parse support' }
SHParserST80 >> leaveBlock [
	arguments
		removeKey: blockDepth
		ifAbsent: [].
	temporaries
		removeKey: blockDepth
		ifAbsent: [].
	blockDepth := blockDepth - 1.
	bracketDepth := bracketDepth - 1
]

{ #category : #scan }
SHParserST80 >> nextChar [
	sourcePosition := sourcePosition + 1.
	^ source
		at: sourcePosition
		ifAbsent: [Character space]
]

{ #category : #parse }
SHParserST80 >> parse [
	^ self parse: self isMethod.
]

{ #category : #parse }
SHParserST80 >> parse: aBoolean [ 
	self initializeInstanceVariables.
	sourcePosition := 1.
	arguments := Dictionary new.
	temporaries := Dictionary new.
	blockDepth := bracketDepth := 0.
	ranges isNil
		ifTrue: [ranges := OrderedCollection new: 100]
		ifFalse: [ranges reset].
	errorBlock := [^ false].
	[self scanNext.
	aBoolean
		ifTrue: [self parseMessagePattern.
			self parsePragmaSequence].
	self parseMethodTemporaries.
	aBoolean
		ifTrue: [self parsePragmaSequence].
	self parseStatementList.
	currentToken
		ifNotNil: [self error]]
		ensure: [errorBlock := nil].
	^ true
]

{ #category : #parse }
SHParserST80 >> parseArray [
	[currentTokenFirst == $)]
		whileFalse: [self parseLiteralArrayElement].
	self scanPast: #arrayEnd
]

{ #category : #parse }
SHParserST80 >> parseBinary [
	| binary type |
	self parseUnary.
	[self isBinary]
		whileTrue: [binary := currentToken.
			type := #binary.
			(binary isEmpty
					or: [Symbol
							hasInterned: binary
							ifTrue: [:sym | nil]])
				ifFalse: [type := (Symbol thatStartsCaseSensitive: binary skipping: nil) isNil
								ifTrue: [#undefinedBinary]
								ifFalse: [#incompleteBinary]].
			self scanPast: type.
			self parseTerm.
			self parseUnary]
]

{ #category : #parse }
SHParserST80 >> parseBinaryMessagePattern [
	self scanPast: #patternBinary.
	self failUnless: self isName.
	self scanPast: #patternArg
]

{ #category : #parse }
SHParserST80 >> parseBlock [
	self enterBlock.
	self scanPast: #blockStart level: bracketDepth.
	currentTokenFirst == $:
		ifTrue: [self parseBlockArguments].
	currentTokenFirst == $|
		ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $].
	self scanPast: #blockEnd level: bracketDepth.
	self leaveBlock
]

{ #category : #parse }
SHParserST80 >> parseBlockArguments [
	[currentTokenFirst == $:]
		whileTrue: [self scanPast: #blockArgColon.
			self failUnless: self isName.
			self scanPast: #blockPatternArg].
	currentTokenFirst == $|
		ifTrue: [^ self scanPast: #blockArgsBar]
]

{ #category : #parse }
SHParserST80 >> parseBlockTemporaries [
	currentTokenFirst == $|
		ifFalse: [ ^ self ].
	self scanPast: #blockTempBar.
	[ self isName ] whileTrue: [ self scanPast: #blockPatternTempVar ].
	self failUnless: currentToken = '|'.
	self scanPast: #blockTempBar
]

{ #category : #parse }
SHParserST80 >> parseBraceArray [
	self parseStatementListForBraceArray.
	self failUnless: currentTokenFirst == $}.
	self scanPast: #rightBrace
]

{ #category : #parse }
SHParserST80 >> parseByteArray [
	[currentTokenFirst == $]]
		whileFalse: [self scanPast: #integer].
	self scanPast: #byteArrayEnd
]

{ #category : #parse }
SHParserST80 >> parseCascade [
	self parseKeyword.
	[currentTokenFirst == $;]
		whileTrue: [self scanPast: #cascadeSeparator.
			self parseKeyword]
]

{ #category : #parse }
SHParserST80 >> parseCharSymbol [
	| s e |
	s := sourcePosition - 1.
	e := sourcePosition.
	self nextChar.
	self
		scanPast: #symbol
		start: s
		end: e
]

{ #category : #parse }
SHParserST80 >> parseExpression [
	| assignType |
	self isName
		ifTrue: [self
				scanPast: (self resolve: currentToken).
			self isAssignment
				ifTrue: [assignType := self isAnsiAssignment
								ifTrue: [#ansiAssignment]
								ifFalse: [#assignment].
					self scanPast: assignType.
					self parseExpression]
				ifFalse: [self parseCascade]]
		ifFalse: [self parseTerm.
			self parseCascade]
]

{ #category : #parse }
SHParserST80 >> parseExternalCall [
	self scanNext.
	self scanPast: #externalCallType.
	currentToken = '*'
		ifTrue: [self scanPast: #externalCallTypePointerIndicator].
	currentTokenFirst isDigit
		ifTrue: [self scanPast: #integer]
		ifFalse: [self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentTokenFirst == $(.
	self scanPast: #leftParenthesis.
	[currentTokenFirst ~~ $)]
		whileTrue: [self scanPast: #externalCallType.
			currentToken = '*'
				ifTrue: [self scanPast: #externalCallTypePointerIndicator]].
	self scanPast: #rightParenthesis.
	currentToken = 'module:'
		ifTrue: [self scanPast: #module.
			self failUnless: currentTokenFirst == $'.
			self parseString].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : #parse }
SHParserST80 >> parseKeyword [
	| keyword rangeIndices |
	self parseBinary.
	keyword := ''.
	rangeIndices := #().
	[[self isKeyword]
		whileTrue: [keyword := keyword , currentToken.
			self rangeType: #keyword.
			rangeIndices := rangeIndices copyWith: ranges size.
			self scanNext.
			self parseTerm.
			self parseBinary].
	nil]
		ensure: [| type | (keyword isEmpty
					or: [Symbol
							hasInterned: keyword
							ifTrue: [:sym | nil]])
				ifFalse: [type := (Symbol thatStartsCaseSensitive: keyword skipping: nil) isNil
								ifTrue: [#undefinedKeyword]
								ifFalse: [#incompleteKeyword].
					rangeIndices
						do: [:i | (ranges at: i)
								type: type]]]
]

{ #category : #parse }
SHParserST80 >> parseKeywordMessagePattern [
	[self isKeyword]
		whileTrue: [self scanPast: #patternKeyword.
			self failUnless: self isName.
			self scanPast: #patternArg]
]

{ #category : #parse }
SHParserST80 >> parseLiteral: inArray [ 
	| pos c |
	currentTokenFirst == $$
		ifTrue: [self failWhen: self currentChar isNil.
			self rangeType: #'$'.
			pos := currentTokenSourcePosition + 1.
			self nextChar.
			^ self
				scanPast: #character
				start: pos
				end: pos].
	currentTokenFirst isDigit
		ifTrue: [^ self scanPast: #number].
	currentToken = '-'
		ifTrue: [c := self currentChar.
			(inArray
					and: [c isNil
							or: [c isDigit not]])
				ifTrue: [^ self scanPast: #symbol].
			self scanPast: #-.
			self failWhen: currentToken isNil.
			^ self scanPast: #number].
	currentTokenFirst == $'
		ifTrue: [^ self parseString].
	currentTokenFirst == $#
		ifTrue: [^ self parseSymbol].
	(inArray
			and: [currentToken notNil])
		ifTrue: [^ self scanPast: #symbol].
	self failWhen: currentTokenFirst == $..
	self error
]

{ #category : #parse }
SHParserST80 >> parseLiteralArrayElement [
	| type |
	(currentTokenFirst isLetter
			or: [currentTokenFirst == $_])
		ifTrue: [type := (#('true' 'false' 'nil' ) includes: currentToken)
						ifTrue: [currentToken asSymbol]
						ifFalse: [#symbol].
			^ self scanPast: type].
	currentTokenFirst == $(
		ifTrue: [self scanPast: #arrayStart.
			^ self parseArray].
	currentTokenFirst == $[
		ifTrue: [self scanPast: #byteArrayStart.
			^ self parseByteArray].
	^ self parseLiteral: true
]

{ #category : #parse }
SHParserST80 >> parseMessagePattern [
	self isName
		ifTrue: [self parseUnaryMessagePattern]
		ifFalse: [self isBinary
				ifTrue: [self parseBinaryMessagePattern]
				ifFalse: [self failUnless: self isKeyword.
					self parseKeywordMessagePattern]]
]

{ #category : #parse }
SHParserST80 >> parseMethodTemporaries [
	currentTokenFirst == $|
		ifFalse: [ ^ self ].
	self scanPast: #methodTempBar.
	[ self isName ] whileTrue: [ self scanPast: #patternTempVar ].
	self failUnless: currentToken = '|'.
	self scanPast: #methodTempBar
]

{ #category : #parse }
SHParserST80 >> parsePragmaBinary [
	self scanPast: #pragmaBinary.
	self isName
		ifTrue: [self
				scanPast: (self resolvePragmaArgument: currentToken)]
		ifFalse: [self parseLiteral: false].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : #parse }
SHParserST80 >> parsePragmaKeyword [
	[self isKeyword]
		whileTrue: [self scanPast: #pragmaKeyword.
			self isName
				ifTrue: [self
						scanPast: (self resolvePragmaArgument: currentToken)]
				ifFalse: [self parseLiteral: false]].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : #parse }
SHParserST80 >> parsePragmaSequence [
	[currentToken = '<']
		whileTrue: [self scanPast: #primitiveOrExternalCallStart.
			currentToken = 'primitive:'
				ifTrue: [self rangeType: #primitive.
					self parsePrimitive]
				ifFalse: [self isTokenExternalFunctionCallingConvention
						ifTrue: [self rangeType: #externalFunctionCallingConvention.
							self parseExternalCall]
						ifFalse: [self isName
								ifTrue: [self scanPast: #pragmaUnary.
									self failUnless: currentToken = '>'.
									self scanPast: #primitiveOrExternalCallEnd]
								ifFalse: [self isKeyword
										ifTrue: [self parsePragmaKeyword]
										ifFalse: [self isBinary
												ifTrue: [self parsePragmaBinary]
												ifFalse: [self error]]]]]]
]

{ #category : #parse }
SHParserST80 >> parsePrimitive [
	self scanNext.
	currentTokenFirst isDigit
		ifTrue: [self scanPast: #integer]
		ifFalse: [self parseStringOrSymbol].
	currentToken = 'module:'
		ifTrue: [self scanPast: #module.
			self parseStringOrSymbol.
			currentToken = 'error:'
				ifTrue: [self scanPast: #primitive.
					self isName
						ifTrue: [self scanPast: #patternTempVar]
						ifFalse: [self parseStringOrSymbol]]]
		ifFalse: [currentToken = 'error:'
				ifTrue: [self scanPast: #module.
					self isName
						ifTrue: [self scanPast: #patternTempVar]
						ifFalse: [self parseStringOrSymbol].
					currentToken = 'module:'
						ifTrue: [self scanPast: #primitive.
							self parseStringOrSymbol]]].
	self failUnless: currentToken = '>'.
	self scanPast: #primitiveOrExternalCallEnd
]

{ #category : #parse }
SHParserST80 >> parseStatement [
	currentTokenFirst == $^
		ifTrue: [self scanPast: #return].
	self parseExpression
]

{ #category : #parse }
SHParserST80 >> parseStatementList [
	[[currentTokenFirst == $.]
		whileTrue: [self scanPast: #statementSeparator].
	(currentToken notNil
			and: [currentTokenFirst ~~ $]])
		ifTrue: [self parseStatement].
	currentTokenFirst == $.]
		whileTrue: [self scanPast: #statementSeparator]
]

{ #category : #parse }
SHParserST80 >> parseStatementListForBraceArray [
	[currentTokenFirst ~~ $}
		ifTrue: [self parseStatement].
	currentTokenFirst == $.]
		whileTrue: [self scanPast: #statementSeparator]
]

{ #category : #parse }
SHParserST80 >> parseString [
	| first c answer last |
	first := sourcePosition.
	answer := ''.
	[(c := self currentChar) isNil
		ifTrue: [self
				rangeType: #unfinishedString
				start: first - 1
				end: source size.
			self error].
	(c ~~ $'
			and: [answer := answer copyWith: c.
				true])
		or: [self peekChar == $'
				and: [sourcePosition := sourcePosition + 1.
					answer := answer copyWith: $'.
					true]]]
		whileTrue: [sourcePosition := sourcePosition + 1].
	last := sourcePosition.
	self nextChar.
	self
		scanPast: #string
		start: first - 1
		end: last.
	^ answer
]

{ #category : #parse }
SHParserST80 >> parseStringOrSymbol [
	currentTokenFirst == $'
		ifTrue: [^ self parseString].
	currentTokenFirst == $#
		ifTrue: [^ self parseSymbol].
	self error
]

{ #category : #parse }
SHParserST80 >> parseSymbol [
	| c |
	currentToken = '#'
		ifTrue: [self rangeType: #symbol.
			self scanWhitespace].
	c := self currentChar.
	self
		failWhen: (c isNil
				or: [c isSeparator]).
	c == $(
		ifTrue: [self nextChar.
			self
				scanPast: #arrayStart
				start: currentTokenSourcePosition
				end: currentTokenSourcePosition + 1.
			^ self parseArray].
	c == $[
		ifTrue: [self nextChar.
			self
				scanPast: #byteArrayStart
				start: currentTokenSourcePosition
				end: currentTokenSourcePosition + 1.
			^ self parseByteArray].
	c == $'
		ifTrue: [^ self parseSymbolString].
	((self isSelectorCharacter: c)
			or: [c == $-])
		ifTrue: [^ self parseSymbolSelector].
	(c isLetter
			or: [c == $:])
		ifTrue: [^ self parseSymbolIdentifier].
	^ self parseCharSymbol
]

{ #category : #parse }
SHParserST80 >> parseSymbolIdentifier [
	| c start end |
	c := self currentChar.
	self
		failUnless: (c isLetter
				or: [c == $:
						or: [c == $_]]).
	start := sourcePosition.
	[c := self nextChar.
	c isAlphaNumeric
		or: [c == $:
				or: [c == $_]]] whileTrue.
	end := sourcePosition - 1.
	c := source copyFrom: start - 1 to: end.
	self
		scanPast: #symbol
		start: start - 1
		end: end.
	^ c
]

{ #category : #parse }
SHParserST80 >> parseSymbolSelector [
	| start end |
	start := sourcePosition - 1.
	end := sourcePosition.
	[self isSelectorCharacter: self nextChar]
		whileTrue: [end := sourcePosition].
	self
		scanPast: #symbol
		start: start
		end: end
]

{ #category : #parse }
SHParserST80 >> parseSymbolString [
	| first c last |
	first := sourcePosition.
	self nextChar.
	[(c := self currentChar) isNil
		ifTrue: [self
				rangeType: #unfinishedString
				start: first
				end: source size.
			self error].
	c ~~ $'
		or: [self peekChar == $'
				and: [sourcePosition := sourcePosition + 1.
					true]]]
		whileTrue: [sourcePosition := sourcePosition + 1].
	last := sourcePosition.
	self nextChar.
	self
		scanPast: #stringSymbol
		start: first - 1
		end: last
]

{ #category : #parse }
SHParserST80 >> parseTerm [
	self failWhen: currentToken isNil.
	currentTokenFirst == $(
		ifTrue: [bracketDepth := bracketDepth + 1.
			self scanPast: #leftParenthesis level: bracketDepth.
			self parseExpression.
			self failUnless: currentTokenFirst == $).
			self scanPast: #rightParenthesis level: bracketDepth.
			^ bracketDepth := bracketDepth - 1].
	currentTokenFirst == $[
		ifTrue: [^ self parseBlock].
	currentTokenFirst == ${
		ifTrue: [self scanPast: #leftBrace.
			^ self parseBraceArray].
	self isName
		ifTrue: [^ self
				scanPast: (self resolve: currentToken)].
	self parseLiteral: false
]

{ #category : #parse }
SHParserST80 >> parseUnary [
	| unary type |
	[self isName]
		whileTrue: [unary := currentToken.
			type := #unary.
			(unary isEmpty
					or: [Symbol
							hasInterned: unary
							ifTrue: [:sym | nil]])
				ifFalse: [type := (Symbol thatStartsCaseSensitive: unary skipping: nil) isNil
								ifTrue: [#undefinedUnary]
								ifFalse: [#incompleteUnary]].
			self scanPast: type]
]

{ #category : #parse }
SHParserST80 >> parseUnaryMessagePattern [
	self scanPast: #patternUnary
]

{ #category : #'parse support' }
SHParserST80 >> parsingBlockarguments [
	^ ranges isEmpty not and:[|last| last:= ranges last.
		last type = #blockPatternArg]
]

{ #category : #scan }
SHParserST80 >> peekChar [
	^ source
		at: sourcePosition + 1
		ifAbsent: [Character space]
]

{ #category : #'parse support' }
SHParserST80 >> pushArgument: aString [ 
	(arguments
		at: blockDepth
		ifAbsentPut: [OrderedCollection new: 10])
		add: aString
]

{ #category : #'parse support' }
SHParserST80 >> pushTemporary: aString [ 
	(temporaries
		at: blockDepth
		ifAbsentPut: [OrderedCollection new: 10])
		add: aString
]

{ #category : #'recording ranges' }
SHParserST80 >> rangeType: aSymbol [ 

	self failWhen: currentToken isNil.
	^ self
		rangeType: aSymbol
		start: currentTokenSourcePosition
		end: currentTokenSourcePosition + currentToken size - 1
]

{ #category : #'recording ranges' }
SHParserST80 >> rangeType: aSymbol start: s end: e [ 
	^ ranges
		add: (SHRange
				start: s
				end: e
				type: aSymbol)
]

{ #category : #parse }
SHParserST80 >> rangesIn: sourceString classOrMetaClass: aBehaviour workspace: aWorkspace environment: anEnvironmentOrNil [ 
	anEnvironmentOrNil
		ifNotNil: [environment := anEnvironmentOrNil].
	self workspace: aWorkspace;
		 classOrMetaClass: aBehaviour;
		 source: sourceString.
	self parse.
	^ ranges
]

{ #category : #'identifier testing' }
SHParserST80 >> resolve: aString [ 
	(#('self' 'super' 'true' 'false' 'nil' 'thisContext' ) includes: aString)
		ifTrue: [^ aString asSymbol].
	(self isBlockTempName: aString)
		ifTrue: [^ #blockTempVar].
	(self isBlockArgName: aString)
		ifTrue: [^ #blockArg].
	(self isMethodTempName: aString)
		ifTrue: [^ #tempVar].
	(self isMethodArgName: aString)
		ifTrue: [^ #methodArg].
	(instanceVariables includes: aString)
		ifTrue: [^ #instVar].
	workspace
		ifNotNil: [(workspace hasBindingOf: aString)
				ifTrue: [^ #workspaceVar]].
	Symbol
		hasInterned: aString
		ifTrue: [:sym | classOrMetaClass isBehavior
				ifTrue: [classOrMetaClass instanceSide withAllSuperclasses
						do: [:c | 
							(c classPool bindingOf: sym)
								ifNotNil: [^ #classVar].
							c sharedPools
								do: [:p | (p bindingOf: sym)
										ifNotNil: [^ #poolConstant]].
							(c environment bindingOf: sym)
								ifNotNil: [^ #globalVar]]]
				ifFalse: [(environment bindingOf: sym)
						ifNotNil: [^ #globalVar]]].
	^ self resolvePartial: aString
]

{ #category : #'identifier testing' }
SHParserST80 >> resolvePartial: aString [ 
	(#('self' 'super' 'true' 'false' 'nil' 'thisContext' )
			anySatisfy: [:each | each beginsWith: aString])
		ifTrue: [^ #incompleteIdentifier].
	(self isIncompleteBlockTempName: aString)
		ifTrue: [^ #incompleteIdentifier].
	(self isIncompleteBlockArgName: aString)
		ifTrue: [^ #incompleteIdentifier].
	(self isIncompleteMethodTempName: aString)
		ifTrue: [^ #incompleteIdentifier].
	(self isIncompleteMethodArgName: aString)
		ifTrue: [^ #incompleteIdentifier].
	(instanceVariables
			anySatisfy: [:each | each beginsWith: aString])
		ifTrue: [^ #incompleteIdentifier].
	workspace
		ifNotNil: [(workspace hasBindingThatBeginsWith: aString)
				ifTrue: [^ #incompleteIdentifier]].
	classOrMetaClass isBehavior
		ifTrue: [classOrMetaClass instanceSide withAllSuperclasses
				do: [:c | 
					(c classPool hasBindingThatBeginsWith: aString)
						ifTrue: [^ #incompleteIdentifier].
					c sharedPools
						do: [:p | 
							(p hasBindingThatBeginsWith: aString)
								ifTrue: [^ #incompleteIdentifier].
							nil].
					(c environment hasBindingThatBeginsWith: aString)
						ifTrue: [^ #incompleteIdentifier].
					nil]]
		ifFalse: [(environment hasBindingThatBeginsWith: aString)
				ifTrue: [^ #incompleteIdentifier]].
	^ #undefinedIdentifier
]

{ #category : #'identifier testing' }
SHParserST80 >> resolvePartialPragmaArgument: aString [ 
	(#('true' 'false' 'nil' )
			anySatisfy: [:each | each beginsWith: aString])
		ifTrue: [^ #incompleteIdentifier].
	classOrMetaClass isBehavior
		ifTrue: [classOrMetaClass instanceSide withAllSuperclasses
				do: [:c | 
					(c environment hasBindingThatBeginsWith: aString)
						ifTrue: [^ #incompleteIdentifier].
					nil]]
		ifFalse: [(environment hasBindingThatBeginsWith: aString)
				ifTrue: [^ #incompleteIdentifier]].
	^ #undefinedIdentifier
]

{ #category : #'identifier testing' }
SHParserST80 >> resolvePragmaArgument: aString [ 
	(#('true' 'false' 'nil' ) includes: aString)
		ifTrue: [^ aString asSymbol].
	Symbol
		hasInterned: aString
		ifTrue: [:sym | classOrMetaClass isBehavior
				ifTrue: [classOrMetaClass instanceSide withAllSuperclasses
						do: [:c | (c environment bindingOf: sym)
								ifNotNil: [^ #globalVar]]]
				ifFalse: [(environment bindingOf: sym)
						ifNotNil: [^ #globalVar]]].
	^ self resolvePartialPragmaArgument: aString
]

{ #category : #scan }
SHParserST80 >> scanBinary [
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := c asString.
	d := self nextChar.
	((self isSelectorCharacter: c)
			or: [c == $:])
		ifFalse: [^ currentToken].
	(c == $:
			and: [d == $=])
		ifTrue: [currentToken := currentToken , d asString.
			self nextChar.
			^ currentToken].
	((c == $|) and:[self parsingBlockarguments])
		ifTrue: [^ currentToken].
	[self isSelectorCharacter: d]
		whileTrue: [currentToken := currentToken , d asString.
			d := self nextChar].
	^ currentToken
]

{ #category : #scan }
SHParserST80 >> scanComment [
	| c s e |
	s := sourcePosition.
	[sourcePosition := sourcePosition + 1.
	(c := self currentChar)
		ifNil: [self
				rangeType: #unfinishedComment
				start: s
				end: source size.
			^ self error].
	c == $"] whileFalse.
	e := sourcePosition.
	s < e
		ifTrue: [self
				rangeType: #comment
				start: s
				end: e].
	self nextChar.
	self scanWhitespace
]

{ #category : #scan }
SHParserST80 >> scanIdentifier [
	| c start |
	start := sourcePosition.
	[(c := self nextChar) isAlphaNumeric
		or: [c == $_]] whileTrue.
	(c == $:
			and: [(self isSelectorCharacter: self peekChar) not])
		ifTrue: [self nextChar].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	currentTokenSourcePosition := start
]

{ #category : #scan }
SHParserST80 >> scanNext [
	self scanWhitespace.
	currentTokenFirst := self currentChar.
	currentTokenFirst isNil
		ifTrue: [currentTokenFirst := Character space.
			currentTokenSourcePosition := nil.
			currentToken := nil.
			^ nil].
	currentTokenFirst isDigit
		ifTrue: [^ self scanNumber].
	(currentTokenFirst isLetter
			or: [currentTokenFirst == $_])
		ifTrue: [^ self scanIdentifier].
	^ self scanBinary
]

{ #category : #scan }
SHParserST80 >> scanNumber [
	| start c nc base |
	start := sourcePosition.
	self skipDigits.
	c := self currentChar.
	c == $r
		ifTrue: [base := Integer
						readFrom: (ReadStream
								on: (source copyFrom: start to: sourcePosition - 1)).
			self peekChar == $-
				ifTrue: [self nextChar].
			self skipBigDigits: base.
			c := self currentChar.
			c == $.
				ifTrue: [(self isBigDigit: self nextChar base: base)
						ifTrue: [self skipBigDigits: base]
						ifFalse: [sourcePosition := sourcePosition - 1]].
			c := self currentChar.
			('deq' includes: c)
				ifTrue: [((nc := self nextChar) isDigit
							or: [nc == $-
									and: [self peekChar isDigit]])
						ifTrue: [self skipDigits]
						ifFalse: [sourcePosition := sourcePosition - 1]].
			c == $s
				ifTrue: [(nc := self nextChar) isDigit
						ifTrue: [self skipDigits]
						ifFalse: [nc isLetter ifTrue: [sourcePosition := sourcePosition - 1]]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^ currentTokenSourcePosition := start].
	c == $s
		ifTrue: [(nc := self nextChar) isDigit
					ifTrue: [self skipDigits]
					ifFalse: [nc isLetter ifTrue: [sourcePosition := sourcePosition - 1]].
			currentToken := source copyFrom: start to: sourcePosition - 1.
			^ currentTokenSourcePosition := start].
	c == $.
		ifTrue: [self nextChar isDigit
				ifTrue: [self skipDigits]
				ifFalse: [sourcePosition := sourcePosition - 1.
					currentToken := source copyFrom: start to: sourcePosition - 1.
					^ currentTokenSourcePosition := start]].
	c := self currentChar.
	('deq' includes: c)
		ifTrue: [((nc := self nextChar) isDigit
					or: [nc == $-
							and: [self peekChar isDigit]])
				ifTrue: [self skipDigits]
				ifFalse: [sourcePosition := sourcePosition - 1]].
	c == $s
		ifTrue: [(nc := self nextChar) isDigit
				ifTrue: [self skipDigits]
				ifFalse: [nc isLetter ifTrue: [sourcePosition := sourcePosition - 1]]].
	currentToken := source copyFrom: start to: sourcePosition - 1.
	^ currentTokenSourcePosition := start
]

{ #category : #scan }
SHParserST80 >> scanPast: rangeType [ 
	rangeType = #blockPatternArg
		ifTrue: [self pushArgument: currentToken].
	rangeType = #blockPatternTempVar
		ifTrue: [self pushTemporary: currentToken].
	rangeType = #patternArg
		ifTrue: [self pushArgument: currentToken].
	rangeType = #patternTempVar
		ifTrue: [self pushTemporary: currentToken].
	^ self rangeType: rangeType;
		 scanNext
]

{ #category : #scan }
SHParserST80 >> scanPast: rangeType level: level [ 
	| cycle typePlusCycle |
	cycle := level <= 1
				ifTrue: [0]
				ifFalse: [level - 2 \\ 7 + 1].
	typePlusCycle := cycle = 0
				ifTrue: [rangeType]
				ifFalse: [(rangeType , cycle asString) asSymbol].
	^ self scanPast: typePlusCycle
]

{ #category : #scan }
SHParserST80 >> scanPast: rangeType start: startInteger end: endInteger [ 
	^ self
		rangeType: rangeType
		start: startInteger
		end: endInteger;
		 scanNext
]

{ #category : #scan }
SHParserST80 >> scanWhitespace [
	| c |
	[c := self currentChar.
	c notNil
		and: [c isSeparator]]
		whileTrue: [sourcePosition := sourcePosition + 1].
	c == $"
		ifTrue: [self scanComment]
]

{ #category : #scan }
SHParserST80 >> skipBigDigits: baseInteger [ 
	[self isBigDigit: self nextChar base: baseInteger] whileTrue
]

{ #category : #scan }
SHParserST80 >> skipDigits [
	[self nextChar isDigit] whileTrue
]

{ #category : #accessing }
SHParserST80 >> source [
	^ source
]

{ #category : #accessing }
SHParserST80 >> source: aString [ 
	source := aString
]

{ #category : #accessing }
SHParserST80 >> workspace: aWorkspace [ 
	workspace := aWorkspace
]
