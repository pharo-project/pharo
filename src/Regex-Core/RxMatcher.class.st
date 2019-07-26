"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
This is a recursive regex matcher. Not strikingly efficient, but simple. Also, keeps track of matched subexpressions.  The life cycle goes as follows:

1. Initialization. Accepts a syntax tree (presumably produced by RxParser) and compiles it into a matcher built of other classes in this category.

2. Matching. Accepts a stream or a string and returns a boolean indicating whether the whole stream or its prefix -- depending on the message sent -- matches the regex.

3. Subexpression query. After a successful match, and before any other match, the matcher may be queried about the range of specific stream (string) positions that matched to certain parenthesized subexpressions of the original expression.

Any number of queries may follow a successful match, and any number or matches may follow a successful initialization.

Note that `matcher' is actually a sort of a misnomer. The actual matcher is a web of Rxm* instances built by RxMatcher during initialization. RxMatcher is just the interface facade of this network.  It is also a builder of it, and also provides a stream-like protocol to easily access the stream being matched.

Instance variables:
	matcher				<RxmLink> The entry point into the actual matcher.
	stream				<Stream> The stream currently being matched against.
	markerPositions		<Array of: Integer> Positions of markers' matches.
	markerCount		<Integer> Number of markers.
	lastResult 			<Boolean> Whether the latest match attempt succeeded or not.
	lastChar			<Character | nil> character last seen in the matcher stream
"
Class {
	#name : #RxMatcher,
	#superclass : #Object,
	#instVars : [
		'matcher',
		'ignoreCase',
		'startOptimizer',
		'stream',
		'markerPositions',
		'markerCount',
		'lastResult'
	],
	#classVars : [
		'Cr',
		'Lf'
	],
	#category : #'Regex-Core-Base'
}

{ #category : #'instance creation' }
RxMatcher class >> for: aRegex [
	"Create and answer a matcher that will match a regular expression
	specified by the syntax tree of which `aRegex' is a root."

	^self for: aRegex ignoreCase: false
]

{ #category : #'instance creation' }
RxMatcher class >> for: aRegex ignoreCase: aBoolean [
	"Create and answer a matcher that will match a regular expression
	specified by the syntax tree of which `aRegex' is a root."

	^self new
		initialize: aRegex
		ignoreCase: aBoolean
]

{ #category : #'instance creation' }
RxMatcher class >> forString: aString [
	"Create and answer a matcher that will match the regular expression
	`aString'."

	^self for: (RxParser new parse: aString)
]

{ #category : #'instance creation' }
RxMatcher class >> forString: aString ignoreCase: aBoolean [
	"Create and answer a matcher that will match the regular expression
	`aString'."

	^self for: (RxParser new parse: aString) ignoreCase: aBoolean
]

{ #category : #'class initialization' }
RxMatcher class >> initialize [
	"RxMatcher initialize"
	Cr := Character cr.
	Lf := Character lf.
]

{ #category : #private }
RxMatcher >> allocateMarker [
	"Answer an integer to use as an index of the next marker."

	markerCount := markerCount + 1.
	^markerCount
]

{ #category : #testing }
RxMatcher >> atBeginningOfLine [

	^self position = 0 or: [self lastChar = Cr]
]

{ #category : #testing }
RxMatcher >> atBeginningOfWord [

	^(self isWordChar: self lastChar) not
		and: [self isWordChar: stream peek]
]

{ #category : #streaming }
RxMatcher >> atEnd [

	^stream atEnd
]

{ #category : #testing }
RxMatcher >> atEndOfLine [

	^self atEnd or: [stream peek = Cr]
]

{ #category : #testing }
RxMatcher >> atEndOfWord [

	^(self isWordChar: self lastChar)
		and: [(self isWordChar: stream peek) not]
]

{ #category : #testing }
RxMatcher >> atWordBoundary [

	^(self isWordChar: self lastChar)
		xor: (self isWordChar: stream peek)
]

{ #category : #accessing }
RxMatcher >> buildFrom: aSyntaxTreeRoot [
	"Private - Entry point of matcher build process."

	markerCount := 0.  "must go before #dispatchTo: !"
	matcher := aSyntaxTreeRoot dispatchTo: self.
	matcher terminateWith: RxmTerminator new
]

{ #category : #'match enumeration' }
RxMatcher >> copy: aString replacingMatchesWith: replacementString [
	"Copy <aString>, except for the matches. Replace each match with <aString>."

	| answer |
	answer := (String new: 40) writeStream.
	self
		copyStream: aString readStream
		to: answer
		replacingMatchesWith: replacementString.
	^answer contents
]

{ #category : #'match enumeration' }
RxMatcher >> copy: aString translatingMatchesUsing: aBlock [
	"Copy <aString>, except for the matches. For each match, evaluate <aBlock> passing the matched substring as the argument.  Expect the block to answer a String, and replace the match with the answer."

	| answer |
	answer := (String new: 40) writeStream.
	self copyStream: aString readStream to: answer translatingMatchesUsing: aBlock.
	^answer contents
]

{ #category : #'match enumeration' }
RxMatcher >> copyStream: aStream to: writeStream replacingMatchesWith: aString [
	"Copy the contents of <aStream> on the <writeStream>, except for the matches. Replace each match with <aString>."

	| searchStart matchStart matchEnd |
	stream := aStream.
	markerPositions := nil.
	[searchStart := aStream position.
	self proceedSearchingStream: aStream] whileTrue:
		[matchStart := (self subBeginning: 1) first.
		matchEnd := (self subEnd: 1) first.
		aStream position: searchStart.
		searchStart to: matchStart - 1 do:
			[:ignoredPos | writeStream nextPut: aStream next].
		writeStream nextPutAll: aString.
		aStream position: matchEnd.
		"Be extra careful about successful matches which consume no input.
		After those, make sure to advance or finish if already at end."
		matchEnd = searchStart ifTrue: 
			[aStream atEnd
				ifTrue:	[^self "rest after end of whileTrue: block is a no-op if atEnd"]
				ifFalse:	[writeStream nextPut: aStream next]]].
	aStream position: searchStart.
	[aStream atEnd] whileFalse: [writeStream nextPut: aStream next]
]

{ #category : #'match enumeration' }
RxMatcher >> copyStream: aStream to: writeStream translatingMatchesUsing: aBlock [
	"Copy the contents of <aStream> on the <writeStream>, except for the matches. For each match, evaluate <aBlock> passing the matched substring as the argument.  Expect the block to answer a String, and write the answer to <writeStream> in place of the match."

	| searchStart matchStart matchEnd match |
	stream := aStream.	
	markerPositions := nil.
	[searchStart := aStream position.
	self proceedSearchingStream: aStream] whileTrue:
		[matchStart := (self subBeginning: 1) first.
		matchEnd := (self subEnd: 1) first.
		aStream position: searchStart.
		searchStart to: matchStart - 1 do:
			[:ignoredPos | writeStream nextPut: aStream next].
		match := (String new: matchEnd - matchStart + 1) writeStream.
		matchStart to: matchEnd - 1 do:
			[:ignoredPos | match nextPut: aStream next].
		writeStream nextPutAll: (aBlock value: match contents).
		"Be extra careful about successful matches which consume no input.
		After those, make sure to advance or finish if already at end."
		matchEnd = searchStart ifTrue: 
			[aStream atEnd
				ifTrue:	[^self "rest after end of whileTrue: block is a no-op if atEnd"]
				ifFalse:	[writeStream nextPut: aStream next]]].
	aStream position: searchStart.
	[aStream atEnd] whileFalse: [writeStream nextPut: aStream next]
]

{ #category : #privileged }
RxMatcher >> currentState [
	"Answer an opaque object that can later be used to restore the
	matcher's state (for backtracking)."

	| origPosition |
	origPosition := stream position.
	^	[stream position: origPosition]
]

{ #category : #private }
RxMatcher >> hookBranchOf: regexNode onto: endMarker [

	"Private - Recurse down the chain of regexes starting at
	regexNode, compiling their branches and hooking their tails 
	to the endMarker node."

	| rest |

	rest := regexNode regex
		ifNil: [ nil ]
		ifNotNil: [ self hookBranchOf: regexNode regex onto: endMarker ].
	^ RxmBranch new
		next:
			( ( regexNode branch dispatchTo: self )
				pointTailTo: endMarker;
				yourself );
		alternative: rest;
		yourself
]

{ #category : #initialization }
RxMatcher >> initialize: syntaxTreeRoot ignoreCase: aBoolean [
	"Compile thyself for the regex with the specified syntax tree.
	See comment and `building' protocol in this class and 
	#dispatchTo: methods in syntax tree components for details 
	on double-dispatch building. 
	The argument is supposedly a RxsRegex."

	ignoreCase := aBoolean.
	self buildFrom: syntaxTreeRoot.
	startOptimizer := RxMatchOptimizer new initialize: syntaxTreeRoot ignoreCase: aBoolean
]

{ #category : #private }
RxMatcher >> isWordChar: aCharacterOrNil [
	"Answer whether the argument is a word constituent character:
	alphanumeric or _."

	^aCharacterOrNil ~~ nil
		and: [aCharacterOrNil isAlphaNumeric]
]

{ #category : #accessing }
RxMatcher >> lastChar [
	^ stream position = 0
		ifFalse: [ stream skip: -1; next ]
]

{ #category : #accessing }
RxMatcher >> lastResult [

	^lastResult
]

{ #category : #private }
RxMatcher >> makeOptional: aMatcher [
	"Private - Wrap this matcher so that the result would match 0 or 1
	occurrences of the matcher."

	| dummy branch |
	dummy := RxmLink new.
	branch := (RxmBranch new beLoopback)
		next: aMatcher;
		alternative: dummy.
	aMatcher pointTailTo: dummy.
	^branch
]

{ #category : #private }
RxMatcher >> makePlus: aMatcher [
	"Private - Wrap this matcher so that the result would match 1 and more
	occurrences of the matcher."

	| loopback |
	loopback := (RxmBranch new beLoopback)
		next: aMatcher.
	aMatcher pointTailTo: loopback.
	^aMatcher
]

{ #category : #private }
RxMatcher >> makeQuantified: anRxmLink min: min max: max [ 
	"Perform recursive poor-man's transformation of the {<min>,<max>} quantifiers."
	| aMatcher |
	
	"<atom>{,<max>}       ==>  (<atom>{1,<max>})?"
	min = 0 ifTrue: [ 
		^ self makeOptional: (self makeQuantified: anRxmLink min: 1 max: max) ].
	
	"<atom>{<min>,}       ==>  <atom>{<min>-1, <min>-1}<atom>+"
	max ifNil: [
		^ (self makeQuantified: anRxmLink min: 1 max: min-1) pointTailTo: (self makePlus: anRxmLink copy) ].
	
	"<atom>{<max>,<max>}  ==>  <atom><atom> ... <atom>"
	min = max 
		ifTrue: [ 
			aMatcher := anRxmLink copy.
			(min-1) timesRepeat: [ aMatcher pointTailTo: anRxmLink copy ].
			^ aMatcher ].

	"<atom>{<min>,<max>}  ==>  <atom>{<min>,<min>}(<atom>{1,<max>-1})?"
	aMatcher := self makeOptional: anRxmLink copy.
	(max - min - 1) timesRepeat: [ 
		 aMatcher := self makeOptional: (anRxmLink copy pointTailTo: aMatcher) ].
	^ (self makeQuantified: anRxmLink min: min max: min) pointTailTo: aMatcher
]

{ #category : #private }
RxMatcher >> makeStar: aMatcher [
	"Private - Wrap this matcher so that the result would match 0 and more
	occurrences of the matcher."

	| dummy detour loopback |
	dummy := RxmLink new.
	detour := RxmBranch new
		next: aMatcher;
		alternative: dummy.
	loopback := (RxmBranch new beLoopback)
		next: aMatcher;
		alternative: dummy.
	aMatcher pointTailTo: loopback.
	^detour
]

{ #category : #privileged }
RxMatcher >> markerPositionAt: anIndex add: position [
	"Remember position of another instance of the given marker."

	(markerPositions at: anIndex) addFirst: position
]

{ #category : #accessing }
RxMatcher >> matches: aString [
	"Match against a string. Return true if the complete String matches.
	If you want to search for occurences anywhere in the String see #search:"

	^self matchesStream: aString readStream
]

{ #category : #'match enumeration' }
RxMatcher >> matchesIn: aString [
	"Search aString repeatedly for the matches of the receiver.  Answer an OrderedCollection of all matches (substrings)."

	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aString readStream
		do: [:match | result add: match].
	^result
]

{ #category : #'match enumeration' }
RxMatcher >> matchesIn: aString collect: aBlock [
	"Search aString repeatedly for the matches of the receiver.  Evaluate aBlock for each match passing the matched substring as the argument, collect evaluation results in an OrderedCollection, and return in. The following example shows how to use this message to split a string into words."
	"'\w+' asRegex matchesIn: 'Now is the Time' collect: [:each | each asLowercase]"

	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aString readStream
		do: [:match | result add: (aBlock value: match)].
	^result
]

{ #category : #'match enumeration' }
RxMatcher >> matchesIn: aString do: aBlock [
	"Search aString repeatedly for the matches of the receiver.
	Evaluate aBlock for each match passing the matched substring
	as the argument."

	self
		matchesOnStream: aString readStream
		do: aBlock
]

{ #category : #'match enumeration' }
RxMatcher >> matchesOnStream: aStream [

	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aStream
		do: [:match | result add: match].
	^result
]

{ #category : #'match enumeration' }
RxMatcher >> matchesOnStream: aStream collect: aBlock [

	| result |
	result := OrderedCollection new.
	self
		matchesOnStream: aStream
		do: [:match | result add: (aBlock value: match)].
	^result
]

{ #category : #'match enumeration' }
RxMatcher >> matchesOnStream: aStream do: aBlock [
	"Be extra careful about successful matches which consume no input.
	After those, make sure to advance or finish if already at end."

	| position subexpression |
	[
		position := aStream position.
		self searchStream: aStream
	] whileTrue: [
		subexpression := self subexpression: 1.
		aBlock value: subexpression.
		subexpression size = 0 ifTrue: [
			aStream atEnd
				ifTrue: [^self]
				ifFalse: [aStream next]]]
]

{ #category : #accessing }
RxMatcher >> matchesPrefix: aString [
	"Match against a string. Return true if a prefix matches.
	If you want to match 
		- the full string use #matches:
		- anywhere in the string use #search:"

	^self matchesStreamPrefix: aString readStream
]

{ #category : #accessing }
RxMatcher >> matchesStream: theStream [
	"Match thyself against a positionable stream."

	^(self matchesStreamPrefix: theStream)
		and: [stream atEnd]
]

{ #category : #accessing }
RxMatcher >> matchesStreamPrefix: theStream [
	"Match thyself against a positionable stream."

	stream := theStream.
	markerPositions := nil.
	^self tryMatch
]

{ #category : #'match enumeration' }
RxMatcher >> matchingRangesIn: aString [
	"Search aString repeatedly for the matches of the receiver.  Answer an OrderedCollection of ranges of each match (index of first character to: index of last character)."

	| result |
	result := OrderedCollection new.
	self
		matchesIn: aString 
		do: [:match | result add: (self position - match size + 1 to: self position)].
	^result
]

{ #category : #streaming }
RxMatcher >> next [
	^ stream next
]

{ #category : #testing }
RxMatcher >> notAtWordBoundary [

	^self atWordBoundary not
]

{ #category : #streaming }
RxMatcher >> position [

	^stream position
]

{ #category : #private }
RxMatcher >> proceedSearchingStream: aStream [

	| position |
	position := aStream position.
	[aStream atEnd] whileFalse:
		[self tryMatch ifTrue: [^true].
		aStream position: position; next.
		position := aStream position].
	"Try match at the very stream end too!"
	self tryMatch ifTrue: [^true]. 
	^false
]

{ #category : #privileged }
RxMatcher >> restoreState: aBlock [

	aBlock value
]

{ #category : #accessing }
RxMatcher >> search: aString [
	"Search anywhere in the String for occurrence of something matching myself.
	If you want to match the full String see #matches:
	Answer a Boolean indicating success."

	^self searchStream: aString readStream
]

{ #category : #accessing }
RxMatcher >> searchStream: aStream [
	"Search the stream for occurrence of something matching myself.
	After the search has occurred, stop positioned after the end of the
	matched substring. Answer a Boolean indicating success."

	| position |
	stream := aStream.
	position := aStream position.
	markerPositions := nil.
	[aStream atEnd] whileFalse:
		[self tryMatch ifTrue: [^true].
		aStream position: position; next.
		position := aStream position].
	"Try match at the very stream end too!"
	self tryMatch ifTrue: [^true]. 
	^false
]

{ #category : #splitjoin }
RxMatcher >> split: aString indicesDo: aBlock [
	| lastPosition |

	stream := aString readStream.
	lastPosition := stream position.
	
	[ self searchStream: stream ] whileTrue: [ 
		aBlock value: lastPosition+1 value: (self subBeginning: 1) first.
		[ lastPosition < stream position ] assertWithDescription: 'Regex cannot match null string'.
		lastPosition := stream position ].
	
	aBlock value: lastPosition + 1 value: aString size.
]

{ #category : #accessing }
RxMatcher >> subBeginning: subIndex [

	^markerPositions at: subIndex * 2 - 1
]

{ #category : #accessing }
RxMatcher >> subEnd: subIndex [

	^markerPositions at: subIndex * 2
]

{ #category : #accessing }
RxMatcher >> subexpression: subIndex [
	"Answer a string that matched the subexpression at the given index.
	If there are multiple matches, answer the last one.
	If there are no matches, answer nil. 
	(NB: it used to answer an empty string but I think nil makes more sense)."

	| matches |
	matches := self subexpressions: subIndex.
	^matches isEmpty ifTrue: [nil] ifFalse: [matches last]
]

{ #category : #accessing }
RxMatcher >> subexpressionCount [

	^markerCount // 2
]

{ #category : #accessing }
RxMatcher >> subexpressions: subIndex [
	"Answer an array of all matches of the subexpression at the given index.
	The answer is always an array; it is empty if there are no matches."

	| originalPosition startPositions stopPositions reply |
	originalPosition := stream position.
	startPositions := self subBeginning: subIndex.
	stopPositions := self subEnd: subIndex.
	(startPositions isEmpty or: [stopPositions isEmpty]) ifTrue: [^Array new].
	reply := OrderedCollection new.
	startPositions with: stopPositions do:
		[:start :stop |
		stream position: start.
		reply add: (stream next: stop - start)].
	stream position: originalPosition.
	^reply asArray
]

{ #category : #testing }
RxMatcher >> supportsSubexpressions [

	^true
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxAny [
	"Double dispatch from the syntax tree. 
	Create a matcher for any non-null character."

	^RxmPredicate new
		predicate: [:char | char asInteger ~= 0]
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxBeginningOfLine [
	"Double dispatch from the syntax tree. 
	Create a matcher for beginning-of-line condition."

	^RxmSpecial new beBeginningOfLine
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxBeginningOfWord [
	"Double dispatch from the syntax tree. 
	Create a matcher for beginning-of-word condition."

	^RxmSpecial new beBeginningOfWord
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxBranch: branchNode [

	"Double dispatch from the syntax tree. 
	Branch node is a link in a chain of concatenated pieces.
	First build the matcher for the rest of the chain, then make 
	it for the current piece and hook the rest to it."

	| result next rest |

	branchNode branch ifNil: [ ^ branchNode piece dispatchTo: self ].	"Optimization: glue a sequence of individual characters into a single string to match."
	branchNode piece isAtomic
		ifTrue: [ result := ( String new: 40 ) writeStream.
			next := branchNode tryMergingInto: result.
			result := result contents.
			result size > 1
				ifTrue: [ "worth merging"
					rest := next notNil
						ifTrue: [ next dispatchTo: self ]
						ifFalse: [ nil ].
					^ ( RxmSubstring new substring: result ignoreCase: ignoreCase )
						pointTailTo: rest;
						yourself
					]
			].	"No optimization possible or worth it, just concatenate all. "
	^ ( branchNode piece dispatchTo: self )
		pointTailTo: ( branchNode branch dispatchTo: self );
		yourself
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxCharSet: charSetNode [
	"Double dispatch from the syntax tree. 
	A character set is a few characters, and we either match any of them,
	or match any that is not one of them."

	^RxmPredicate with: (charSetNode predicateIgnoringCase: ignoreCase)
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxCharacter: charNode [
	"Double dispatch from the syntax tree. 
	We get here when no merging characters into strings was possible."

	| wanted |
	wanted := charNode character.
	^RxmPredicate new predicate: 
		(ignoreCase
			ifTrue: [[:char | char sameAs: wanted]]
			ifFalse: [[:char | char = wanted]])
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxEndOfLine [
	"Double dispatch from the syntax tree. 
	Create a matcher for end-of-line condition."

	^RxmSpecial new beEndOfLine
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxEndOfWord [
	"Double dispatch from the syntax tree. 
	Create a matcher for end-of-word condition."

	^RxmSpecial new beEndOfWord
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxEpsilon [
	"Double dispatch from the syntax tree. Match empty string. This is unlikely
	to happen in sane expressions, so we'll live without special epsilon-nodes."

	^RxmSubstring new
		substring: String new
		ignoreCase: ignoreCase
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxLookaround: lookaroundNode [
	"Double dispatch from the syntax tree. 
	Special link can handle lookarounds (look ahead, positive and negative)."
	| piece |
	piece := lookaroundNode piece dispatchTo: self.
	^ RxmLookahaed with: piece
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxMessagePredicate: messagePredicateNode [
	"Double dispatch from the syntax tree. 
	Special link can handle predicates."

	^messagePredicateNode negated
		ifTrue: [RxmPredicate new bePerformNot: messagePredicateNode selector]
		ifFalse: [RxmPredicate new bePerform: messagePredicateNode selector]
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxNonWordBoundary [
	"Double dispatch from the syntax tree. 
	Create a matcher for the word boundary condition."

	^RxmSpecial new beNotWordBoundary
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxPiece: pieceNode [
	"Double dispatch from the syntax tree. 
	Piece is an atom repeated a few times. Take care of a special
	case when the atom is repeated just once."

	| atom |
	atom := pieceNode atom dispatchTo: self.
	^pieceNode isSingular
		ifTrue: [atom]
		ifFalse: [pieceNode isStar
			ifTrue: [self makeStar: atom]
			ifFalse: [pieceNode isPlus
				ifTrue: [self makePlus: atom]
				ifFalse: [pieceNode isOptional
					ifTrue: [self makeOptional: atom]
					ifFalse: [self makeQuantified: atom min: pieceNode min max: pieceNode max]]]]
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxPredicate: predicateNode [
	"Double dispatch from the syntax tree. 
	A character set is a few characters, and we either match any of them,
	or match any that is not one of them."

	^RxmPredicate with: predicateNode predicate
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxRegex: regexNode [
	"Double dispatch from the syntax tree. 
	Regex node is a chain of branches to be tried. Should compile this 
	into a bundle of parallel branches, between two marker nodes." 
	
	| startIndex endIndex endNode alternatives |
	startIndex := self allocateMarker.
	endIndex := self allocateMarker.
	endNode := RxmMarker new index: endIndex.
	alternatives := self hookBranchOf: regexNode onto: endNode.
	^(RxmMarker new index: startIndex)
		pointTailTo: alternatives;
		yourself
]

{ #category : #'double dispatch' }
RxMatcher >> syntaxWordBoundary [
	"Double dispatch from the syntax tree. 
	Create a matcher for the word boundary condition."

	^RxmSpecial new beWordBoundary
]

{ #category : #private }
RxMatcher >> tryMatch [

	"Match thyself against the current stream."

	| oldMarkerPositions |

	oldMarkerPositions := markerPositions.
	markerPositions := Array new: markerCount.
	1 to: markerCount do: [ :i | markerPositions at: i put: OrderedCollection new ].
	lastResult := startOptimizer
		ifNil: [ matcher matchAgainst: self ]
		ifNotNil: [ ( startOptimizer canStartMatch: stream peek in: self ) and: [ matcher matchAgainst: self ] ].	"check for duplicates"
	( lastResult not
		or: [ oldMarkerPositions isNil or: [ oldMarkerPositions size ~= markerPositions size ] ] )
		ifTrue: [ ^ lastResult ].
	oldMarkerPositions
		with: markerPositions
		do: [ :oldPos :newPos | 
			oldPos size = newPos size
				ifFalse: [ ^ lastResult ].
			oldPos
				with: newPos
				do: [ :old :new | 
					old = new
						ifFalse: [ ^ lastResult ]
					]
			].	"this is a duplicate"
	^ lastResult := false
]
