"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
This represents a character that satisfies a certain predicate.

Instance Variables:

	predicate	<BlockClosure>	A one-argument block. If it evaluates to the value defined by <negated> when it is passed a character, the predicate is considered to match.
	negation	<BlockClosure>	A one-argument block that is a negation of <predicate>.
"
Class {
	#name : #RxsPredicate,
	#superclass : #RxsNode,
	#instVars : [
		'predicate',
		'negation'
	],
	#classVars : [
		'EscapedLetterSelectors',
		'NamedClassSelectors'
	],
	#category : #'Regex-Core-Nodes'
}

{ #category : #'instance creation' }
RxsPredicate class >> forEscapedLetter: aCharacter [

	^self new perform:
		(EscapedLetterSelectors
			at: aCharacter
			ifAbsent: [RxParser signalSyntaxException: 'bad backslash escape'])
]

{ #category : #'instance creation' }
RxsPredicate class >> forNamedClass: aString [

	^self new perform:
		(NamedClassSelectors
			at: aString
			ifAbsent: [RxParser signalSyntaxException: 'bad character class name'])
]

{ #category : #'class initialization' }
RxsPredicate class >> initialize [
	"self initialize"

	self
		initializeNamedClassSelectors;
		initializeEscapedLetterSelectors
]

{ #category : #'class initialization' }
RxsPredicate class >> initializeEscapedLetterSelectors [
	"self initializeEscapedLetterSelectors"

	(EscapedLetterSelectors := Dictionary new)
		at: $w put: #beWordConstituent;
		at: $W put: #beNotWordConstituent;
		at: $d put: #beDigit;
		at: $D put: #beNotDigit;
		at: $s put: #beSpace;
		at: $S put: #beNotSpace;
		at: $\ put: #beBackslash
]

{ #category : #'class initialization' }
RxsPredicate class >> initializeNamedClassSelectors [
	"self initializeNamedClassSelectors"

	(NamedClassSelectors := Dictionary new)
		at: 'alnum' put: #beAlphaNumeric;
		at: 'alpha' put: #beAlphabetic;
		at: 'cntrl' put: #beControl;
		at: 'digit' put: #beDigit;
		at: 'graph' put: #beGraphics;
		at: 'lower' put: #beLowercase;
		at: 'print' put: #bePrintable;
		at: 'punct' put: #bePunctuation;
		at: 'space' put: #beSpace;
		at: 'upper' put: #beUppercase;
		at: 'xdigit' put: #beHexDigit
]

{ #category : #'initialize-release' }
RxsPredicate >> beAlphaNumeric [

	predicate := [:char | char isAlphaNumeric].
	negation := [:char | char isAlphaNumeric not]
]

{ #category : #'initialize-release' }
RxsPredicate >> beAlphabetic [

	predicate := [:char | char isLetter].
	negation := [:char | char isLetter not]
]

{ #category : #initialization }
RxsPredicate >> beBackslash [

	predicate := [:char | char == $\].
	negation := [:char | char ~~ $\]
]

{ #category : #initialization }
RxsPredicate >> beControl [

	predicate := [:char | char asInteger < 32].
	negation := [:char | char asInteger >= 32]
]

{ #category : #initialization }
RxsPredicate >> beDigit [

	predicate := [:char | char isDigit].
	negation := [:char | char isDigit not]
]

{ #category : #initialization }
RxsPredicate >> beGraphics [

	self
		beControl;
		negate
]

{ #category : #initialization }
RxsPredicate >> beHexDigit [

	| hexLetters |
	hexLetters := 'abcdefABCDEF'.
	predicate := [:char | char isDigit or: [hexLetters includes: char]].
	negation := [:char | char isDigit not and: [(hexLetters includes: char) not]]
]

{ #category : #initialization }
RxsPredicate >> beLowercase [

	predicate := [:char | char isLowercase].
	negation := [:char | char isLowercase not]
]

{ #category : #'initialize-release' }
RxsPredicate >> beNotDigit [

	self
		beDigit;
		negate
]

{ #category : #initialization }
RxsPredicate >> beNotSpace [

	self
		beSpace;
		negate
]

{ #category : #initialization }
RxsPredicate >> beNotWordConstituent [

	self
		beWordConstituent;
		negate
]

{ #category : #initialization }
RxsPredicate >> bePrintable [

	self
		beControl;
		negate
]

{ #category : #initialization }
RxsPredicate >> bePunctuation [

	| punctuationChars |
	punctuationChars := #($. $, $! $? $; $: $" $' $- $( $) $`).
	predicate := [:char | punctuationChars includes: char].
	negation := [:char | (punctuationChars includes: char) not]
]

{ #category : #'initialize-release' }
RxsPredicate >> beSpace [

	predicate := [:char | char isSeparator].
	negation := [:char | char isSeparator not]
]

{ #category : #initialization }
RxsPredicate >> beUppercase [

	predicate := [:char | char isUppercase].
	negation := [:char | char isUppercase not]
]

{ #category : #'initialize-release' }
RxsPredicate >> beWordConstituent [

	predicate := [:char | char isAlphaNumeric or: [char == $_]].
	negation := [:char | char isAlphaNumeric not and: [char ~~ $_]]
]

{ #category : #accessing }
RxsPredicate >> dispatchTo: anObject [

	^anObject syntaxPredicate: self
]

{ #category : #testing }
RxsPredicate >> isEnumerable [

	^false
]

{ #category : #private }
RxsPredicate >> negate [

	| tmp |
	tmp := predicate.
	predicate := negation.
	negation := tmp
]

{ #category : #accessing }
RxsPredicate >> negated [

	^self copy negate
]

{ #category : #accessing }
RxsPredicate >> predicate [

	^predicate
]

{ #category : #accessing }
RxsPredicate >> predicateNegation [

	^negation
]

{ #category : #accessing }
RxsPredicate >> value: aCharacter [

	^predicate value: aCharacter
]
