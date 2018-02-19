"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
The regular expression parser. Translates a regular expression read from a stream into a parse tree. ('accessing' protocol). The tree can later be passed to a matcher initialization method.  All other classes in this category implement the tree. Refer to their comments for any details.

Instance variables:
	input		<Stream> A stream with the regular expression being parsed.
	lookahead	<Character>
"
Class {
	#name : #RxParser,
	#superclass : #Object,
	#instVars : [
		'input',
		'lookahead'
	],
	#classVars : [
		'BackslashConstants',
		'BackslashSpecials'
	],
	#category : #'Regex-Core'
}

{ #category : #'exception signaling' }
RxParser class >> doHandlingMessageNotUnderstood: aBlock [
	"MNU should be trapped and resignaled as a match error in a few places in the matcher.
	This method factors out this dialect-dependent code to make porting easier."
	^ aBlock
		on: MessageNotUnderstood
		do: [:ex | RxParser signalMatchException: 'invalid predicate selector']
]

{ #category : #'class initialization' }
RxParser class >> initialize [
	"self initialize"
	self
		initializeBackslashConstants;
		initializeBackslashSpecials
]

{ #category : #'class initialization' }
RxParser class >> initializeBackslashConstants [
	"self initializeBackslashConstants"

	(BackslashConstants := Dictionary new)
		at: $e put: Character escape;
		at: $n put: Character lf;
		at: $r put: Character cr;
		at: $f put: Character newPage;
		at: $t put: Character tab
]

{ #category : #'class initialization' }
RxParser class >> initializeBackslashSpecials [
	"Keys are characters that normally follow a \, the values are
	associations of classes and initialization selectors on the instance side
	of the classes."
	"self initializeBackslashSpecials"

	(BackslashSpecials := Dictionary new)
		at: $w put: (Association key: RxsPredicate value: #beWordConstituent);
		at: $W put: (Association key: RxsPredicate value: #beNotWordConstituent);
		at: $s put: (Association key: RxsPredicate value: #beSpace);
		at: $S put: (Association key: RxsPredicate value: #beNotSpace);
		at: $d put: (Association key: RxsPredicate value: #beDigit);
		at: $D put: (Association key: RxsPredicate value: #beNotDigit);
		at: $b put: (Association key: RxsContextCondition value: #beWordBoundary);
		at: $B put: (Association key: RxsContextCondition value: #beNonWordBoundary);
		at: $< put: (Association key: RxsContextCondition value: #beBeginningOfWord);
		at: $> put: (Association key: RxsContextCondition value: #beEndOfWord)
]

{ #category : #utilities }
RxParser class >> parse: aString [
	"Parse the argument and return the result (the parse tree).
	In case of a syntax error, the corresponding exception is signaled."

	^self new parse: aString
]

{ #category : #preferences }
RxParser class >> preferredMatcherClass [
	"The matcher to use. For now just one is available, but in
	principle this determines the matchers built implicitly,
	such as by String>>asRegex, or String>>matchesRegex:.
	This might seem a bit strange place for this preference, but
	Parser is still more or less `central' thing in the whole package."

	^RxMatcher
]

{ #category : #utilities }
RxParser class >> safelyParse: aString [
	"Parse the argument and return the result (the parse tree).
	In case of a syntax error, return nil.
	Exception handling here is dialect-dependent."
	^ [self new parse: aString] on: RegexSyntaxError do: [:ex | nil]
]

{ #category : #'exception signaling' }
RxParser class >> signalCompilationException: errorString [
	RegexCompilationError new signal: errorString
]

{ #category : #'exception signaling' }
RxParser class >> signalMatchException: errorString [
	RegexMatchingError new signal: errorString
]

{ #category : #'exception signaling' }
RxParser class >> signalSyntaxException: errorString [
	RegexSyntaxError new signal: errorString
]

{ #category : #'exception signaling' }
RxParser class >> signalSyntaxException: errorString at: errorPosition [
	RegexSyntaxError signal: errorString at: errorPosition
]

{ #category : #'recursive descent' }
RxParser >> atom [
	"An atom is one of a lot of possibilities, see below."

	| atom |
	(lookahead = #epsilon 
	or: [ lookahead = $| 
	or: [ lookahead = $)
	or: [ lookahead = $*
	or: [ lookahead = $+ 
	or: [ lookahead = $? ]]]]])
		ifTrue: [ ^RxsEpsilon new ].
		
	lookahead = $( 
		ifTrue: [
			"<atom> ::= '(' <regex> ')' "
			self match: $(.
			atom := self regex.
			self match: $).
			^atom ].
	
	lookahead = $[
		ifTrue: [
			"<atom> ::= '[' <characterSet> ']' "
			self match: $[.
			atom := self characterSet.
			self match: $].
			^atom ].
	
	lookahead = $: 
		ifTrue: [
			"<atom> ::= ':' <messagePredicate> ':' "
			self match: $:.
			atom := self messagePredicate.
			self match: $:.
			^atom ].
	
	lookahead = $. 
		ifTrue: [
			"any non-whitespace character"
			self next.
			^RxsContextCondition new beAny].
	
	lookahead = $^ 
		ifTrue: [
			"beginning of line condition"
			self next.
			^RxsContextCondition new beBeginningOfLine].
	
	lookahead = $$ 
		ifTrue: [
			"end of line condition"
			self next.
			^RxsContextCondition new beEndOfLine].
		
	lookahead = $\ 
		ifTrue: [
			"<atom> ::= '\' <character>"
			self next.
			lookahead = #epsilon 
				ifTrue: [ self signalParseError: 'bad quotation' ].
			(BackslashConstants includesKey: lookahead)
				ifTrue: [
					atom := RxsCharacter with: (BackslashConstants at: lookahead).
					self next.
					^atom].
			self ifSpecial: lookahead
				then: [:node | self next. ^node]].
		
	"If passed through the above, the following is a regular character."
	atom := RxsCharacter with: lookahead.
	self next.
	^atom
]

{ #category : #'recursive descent' }
RxParser >> branch [
	"<branch> ::= e | <piece> <branch>"

	| piece branch |
	piece := self piece.
	(lookahead = #epsilon 
	or: [ lookahead = $| 
	or: [ lookahead = $) ]])
		ifTrue: [ branch := nil ]
		ifFalse: [ branch := self branch ].
	^RxsBranch new 
		initializePiece: piece 
		branch: branch
]

{ #category : #'recursive descent' }
RxParser >> characterSet [
	"Match a range of characters: something between `[' and `]'.
	Opening bracked has already been seen, and closing should
	not be consumed as well. Set spec is as usual for
	sets in regexes."

	| spec errorMessage |
	errorMessage := ' no terminating "]"'.
	spec := self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage.
	(spec isEmpty 
	or: [spec = '^']) 
		ifTrue: [
			"This ']' was literal." 
			self next.
			spec := spec, ']', (self inputUpTo: $] nestedOn: $[ errorMessage: errorMessage)].
	^self characterSetFrom: spec
]

{ #category : #private }
RxParser >> characterSetFrom: setSpec [
	"<setSpec> is what goes between the brackets in a charset regex
	(a String). Make a string containing all characters the spec specifies.
	Spec is never empty."

	| negated spec |
	spec := ReadStream on: setSpec.
	spec peek = $^
		ifTrue: 	[negated := true.
				spec next]
		ifFalse:	[negated := false].
	^RxsCharSet new
		initializeElements: (RxCharSetParser on: spec) parse
		negated: negated
]

{ #category : #private }
RxParser >> ifSpecial: aCharacter then: aBlock [
	"If the character is such that it defines a special node when follows a $\,
	then create that node and evaluate aBlock with the node as the parameter.
	Otherwise just return."

	| classAndSelector |
	classAndSelector := BackslashSpecials at: aCharacter ifAbsent: [^self].
	^aBlock value: (classAndSelector key new perform: classAndSelector value)
]

{ #category : #private }
RxParser >> inputUpTo: aCharacter errorMessage: aString [
	"Accumulate input stream until <aCharacter> is encountered
	and answer the accumulated chars as String, not including
	<aCharacter>. Signal error if end of stream is encountered,
	passing <aString> as the error description."

	| accumulator |
	accumulator := WriteStream on: (String new: 20).
	[lookahead ~= aCharacter and: [lookahead ~= #epsilon]]
		whileTrue: [
			accumulator nextPut: lookahead.
			self next].
	lookahead = #epsilon
		ifTrue: [ self signalParseError: aString ].
	^accumulator contents
]

{ #category : #private }
RxParser >> inputUpTo: aCharacter nestedOn: anotherCharacter errorMessage: aString [ 
	"Accumulate input stream until <aCharacter> is encountered
	and answer the accumulated chars as String, not including
	<aCharacter>. Signal error if end of stream is encountered,
	passing <aString> as the error description."

	| accumulator nestLevel |
	accumulator := WriteStream on: (String new: 20).
	nestLevel := 0.
	[lookahead ~= aCharacter or: [nestLevel > 0]] whileTrue: 
			[#epsilon = lookahead ifTrue: [self signalParseError: aString].
			accumulator nextPut: lookahead.
			lookahead = anotherCharacter ifTrue: [nestLevel := nestLevel + 1].
			lookahead = aCharacter ifTrue: [nestLevel := nestLevel - 1].
			self next].
	^accumulator contents
]

{ #category : #private }
RxParser >> inputUpToAny: aDelimiterString errorMessage: aString [
	"Accumulate input stream until any character from <aDelimiterString> is encountered
	and answer the accumulated chars as String, not including the matched characters from the
	<aDelimiterString>. Signal error if end of stream is encountered,
	passing <aString> as the error description."

	| accumulator |
	accumulator := WriteStream on: (String new: 20).
	[(aDelimiterString includes: lookahead) not and: [lookahead ~= #epsilon]]
		whileTrue: [
			accumulator nextPut: lookahead.
			self next ].
	lookahead = #epsilon
		ifTrue: [ self signalParseError: aString ].
	^accumulator contents
]

{ #category : #'recursive descent' }
RxParser >> lookAround [
	"Parse a lookaround expression after: (?<lookround>) 
	<lookround> ::= !<regex> | =<regex>"
	| lookaround |
	(lookahead = $!
	or: [ lookahead = $=])
		ifFalse: [ ^ self signalParseError: 'Invalid lookaround expression ?', lookahead asString ].
	self next.
	lookaround := RxsLookaround with: self regex.
	lookahead = $!
		ifTrue: [ lookaround beNegative ].
	^ lookaround
	
]

{ #category : #private }
RxParser >> match: aCharacter [
	"<aCharacter> MUST match the current lookeahead.
	If this is the case, advance the input. Otherwise, blow up."

	aCharacter ~= lookahead 
		ifTrue: [^self signalParseError].	"does not return"
	self next
]

{ #category : #'recursive descent' }
RxParser >> messagePredicate [
	"Match a message predicate specification: a selector (presumably
	understood by a Character) enclosed in :'s ."

	| spec negated |
	spec := self inputUpTo: $: errorMessage: ' no terminating ":"'.
	negated := false.
	spec first = $^ 
		ifTrue: [
			negated := true.
			spec := spec copyFrom: 2 to: spec size].
	^RxsMessagePredicate new 
		initializeSelector: spec asSymbol
		negated: negated
]

{ #category : #private }
RxParser >> next [
	"Advance the input storing the just read character
	as the lookahead."

	input atEnd
		ifTrue: [lookahead := #epsilon]
		ifFalse: [lookahead := input next]
]

{ #category : #accessing }
RxParser >> parse: aString [
	"Parse input from a string <aString>.
	On success, answers an RxsRegex -- parse tree root.
	On error, raises `RxParser syntaxErrorSignal' with the current
	input stream position as the parameter."

	^self parseStream: (ReadStream on: aString)
]

{ #category : #accessing }
RxParser >> parseStream: aStream [
	"Parse an input from a character stream <aStream>.
	On success, answers an RxsRegex -- parse tree root.
	On error, raises `RxParser syntaxErrorSignal' with the current
	input stream position as the parameter."

	| tree |
	input := aStream.
	lookahead := nil.
	self match: nil.
	tree := self regex.
	self match: #epsilon.
	^tree
]

{ #category : #'recursive descent' }
RxParser >> piece [
	"<piece> ::= <atom> | <atom>* | <atom>+ | <atom>? | <atom>{<number>,<number>}"

	| atom |
	atom := self atom.
	
	lookahead = $*
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ self signalNullableClosureParserError ].
			^ RxsPiece new initializeStarAtom: atom ].

	lookahead = $+
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ self signalNullableClosureParserError ].
			^ RxsPiece new initializePlusAtom: atom ].

	lookahead = $?
		ifTrue: [ 
			self next.
			atom isNullable
				ifTrue: [ 
					^ self lookAround ].
			^ RxsPiece new initializeOptionalAtom: atom ].
	
	lookahead = ${
		ifTrue: [
			^ self quantifiedAtom: atom ].
		
	^ RxsPiece new initializeAtom: atom
]

{ #category : #'recursive descent' }
RxParser >> quantifiedAtom: atom [
	"Parse a quanitifer expression which can have one of the following forms
		{<min>,<max>}    match <min> to <max> occurences
		{<minmax>}       which is the same as with repeated limits: {<number>,<number>}
		{<min>,}         match at least <min> occurences
		{,<max>}         match maximally <max> occurences, which is the same as {0,<max>}"
	| min max |
	self next.
	lookahead = $,
		ifTrue: [ min := 0 ]
		ifFalse: [
			max := min := (self inputUpToAny: ',}' errorMessage: ' no terminating "}"') asUnsignedInteger ].
	lookahead = $,
		ifTrue: [
			self next.
			max := (self inputUpToAny: ',}' errorMessage: ' no terminating "}"') asUnsignedInteger ].	
	self match: $}.
	atom isNullable
		ifTrue: [ self signalNullableClosureParserError ].
	(max isNotNil and: [ max < min ])
		ifTrue: [ self signalParseError: ('wrong quantifier, expected ', min asString, ' <= ', max asString) ].
	^ RxsPiece new 
		initializeAtom: atom
		min: min
		max: max
]

{ #category : #'recursive descent' }
RxParser >> regex [
	"<regex> ::= e | <branch> `|' <regex>"

	| branch regex |
	branch := self branch.
	
	(lookahead = #epsilon 
	or: [ lookahead = $) ])
		ifTrue: [ regex := nil ]
		ifFalse: [
			self match: $|.
			regex := self regex ].
		
	^RxsRegex new initializeBranch: branch regex: regex
]

{ #category : #private }
RxParser >> signalNullableClosureParserError [
	self signalParseError: ' nullable closure'.
]

{ #category : #private }
RxParser >> signalParseError [

	self class 
		signalSyntaxException: 'Regex syntax error' at: input position
]

{ #category : #private }
RxParser >> signalParseError: aString [

	self class signalSyntaxException: aString at: input position
]
