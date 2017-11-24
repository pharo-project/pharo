"
Divides an address into tokens, as specified in RFC 822.  Used by MailAddressParser.
"
Class {
	#name : #MailAddressTokenizer,
	#superclass : #Stream,
	#instVars : [
		'cachedToken',
		'text',
		'pos'
	],
	#classVars : [
		'CSNonAtom',
		'CSNonSeparators',
		'CSParens',
		'CSSpecials'
	],
	#category : #'Network-Mail'
}

{ #category : #'instance creation' }
MailAddressTokenizer class >> forString: aString [
	^self basicNew initialize: aString
]

{ #category : #initialization }
MailAddressTokenizer class >> initialize [
	"Initalize class variables using   MailAddressTokenizer initialize"

	| atomChars |

	CSParens := CharacterSet empty.
	CSParens addAll: '()'.

	CSSpecials := CharacterSet empty.
	CSSpecials addAll: '()<>@,;:\".[]'.

	CSNonSeparators := CharacterSet separators complement.


	"(from RFC 2822)"
	atomChars := CharacterSet empty.
	atomChars addAll: ($A to: $Z).
	atomChars addAll: ($a to: $z).
	atomChars addAll: ($0 to: $9).
	atomChars addAll: '!#$%&''*+-/=?^_`{|}~'.

	CSNonAtom :=  atomChars complement.
]

{ #category : #'instance creation' }
MailAddressTokenizer class >> tokensIn: aString [
	"return a collection of the tokens in aString"
	^(self forString: aString) upToEnd
]

{ #category : #'stream protocol' }
MailAddressTokenizer >> atEnd [
	^ self peek isNil
]

{ #category : #tokenizing }
MailAddressTokenizer >> atEndOfChars [
	^pos > text size
]

{ #category : #initialization }
MailAddressTokenizer >> initialize: aString [
	text := aString.
	pos := 1.
]

{ #category : #'stream protocol' }
MailAddressTokenizer >> next [
	| ans |
	cachedToken ifNil: [ ^self nextToken ].
	ans := cachedToken.
	cachedToken := nil.
	^ans
]

{ #category : #tokenizing }
MailAddressTokenizer >> nextAtom [
	| start end |
	start := pos.
	pos := text indexOfAnyOf: CSNonAtom startingAt: start ifAbsent: [ text size + 1].
	end := pos - 1.
	^MailAddressToken
		type: #Atom
		text: (text copyFrom: start to: end)
]

{ #category : #tokenizing }
MailAddressTokenizer >> nextChar [
	self atEndOfChars ifTrue: [ ^nil ].
	pos := pos + 1.
	^text at: (pos-1)
]

{ #category : #tokenizing }
MailAddressTokenizer >> nextComment [
	| start nestLevel paren |
	start := pos.
	pos := pos + 1.
	nestLevel := 1.
	[ nestLevel > 0 ]
		whileTrue: [ 
			pos := text indexOfAnyOf: CSParens startingAt: pos ifAbsent: [ 0 ].
			pos = 0
				ifTrue: [ self error: 'unterminated comment.  ie, more (''s than )''s' ].
			paren := self nextChar.
			nestLevel := paren = $(
				ifTrue: [ nestLevel + 1 ]
				ifFalse: [ nestLevel - 1 ] ].
	^ MailAddressToken type: #Comment text: (text copyFrom: start to: pos - 1)
]

{ #category : #tokenizing }
MailAddressTokenizer >> nextDomainLiteral [
	| start end |
	start := pos.
	end := text indexOf: $] startingAt: start ifAbsent: [ 0 ].
	end = 0 ifTrue: [
		"not specified"
		self error: 'saw [ without a matching ]' ].

	pos := end+1.

	^MailAddressToken
		type: #DomainLiteral
		text: (text copyFrom: start to: end)
]

{ #category : #tokenizing }
MailAddressTokenizer >> nextQuotedString [
	| res c |
	res := String new writeStream.
	res nextPut: self nextChar.   "record the starting quote"
	[ self atEndOfChars ] whileFalse: [
		c := self nextChar.
		c = $\ ifTrue: [
			res nextPut: c.
			res nextPut: self nextChar ]
		ifFalse: [
			c = $" ifTrue: [
				res nextPut: c.
				^MailAddressToken type: #QuotedString  text: res contents ]
			ifFalse: [
				res nextPut: c ] ] ].

	"hmm, never saw the final quote mark"
	^MailAddressToken type: #QuotedString  text: (res contents, '"')
]

{ #category : #tokenizing }
MailAddressTokenizer >> nextSpecial [
	| c |
	c := self nextChar.
	^MailAddressToken type: c  text: c asString.
]

{ #category : #tokenizing }
MailAddressTokenizer >> nextToken [
	| c |
	self skipSeparators.
	c := self peekChar.
	c ifNil: [ ^nil ].
	c = $( ifTrue: [ ^self nextComment ].
	c = $" ifTrue: [ ^self nextQuotedString ].
	c = $[ ifTrue: [ ^self nextDomainLiteral ].
	(CSSpecials includes: c) ifTrue: [ ^self nextSpecial ].
	^self nextAtom
]

{ #category : #'stream protocol' }
MailAddressTokenizer >> peek [
	cachedToken ifNil: [ cachedToken := self nextToken. ].
	
	^cachedToken	
]

{ #category : #tokenizing }
MailAddressTokenizer >> peekChar [
	^text at: pos ifAbsent: [ nil ]
]

{ #category : #tokenizing }
MailAddressTokenizer >> skipSeparators [
	pos := text indexOfAnyOf: CSNonSeparators  startingAt: pos  ifAbsent: [ text size + 1 ].
]
