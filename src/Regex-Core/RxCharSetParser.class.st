"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
I am a parser created to parse the insides of a character set ([...]) construct. I create and answer a collection of ""elements"", each being an instance of one of: RxsCharacter, RxsRange, or RxsPredicate.

Instance Variables:

	source <Stream>	open on whatever is inside the square brackets we have to parse.
	lookahead	<Character>	The current lookahead character
	elements	<Collection of: <RxsCharacter|RxsRange|RxsPredicate>> Parsing result
"
Class {
	#name : #RxCharSetParser,
	#superclass : #Object,
	#instVars : [
		'source',
		'lookahead',
		'elements'
	],
	#category : #'Regex-Core-Utilities'
}

{ #category : #'instance creation' }
RxCharSetParser class >> on: aStream [

	^self new initialize: aStream
]

{ #category : #parsing }
RxCharSetParser >> addChar: aChar [

	elements add: (RxsCharacter with: aChar)
]

{ #category : #parsing }
RxCharSetParser >> addRangeFrom: firstChar to: lastChar [

	firstChar asInteger > lastChar asInteger ifTrue:
		[RxParser signalSyntaxException: ' bad character range' at: source position].
	elements add: (RxsRange from: firstChar to: lastChar)
]

{ #category : #initialization }
RxCharSetParser >> initialize: aStream [

	source := aStream.
	lookahead := aStream next.
	elements := OrderedCollection new
]

{ #category : #parsing }
RxCharSetParser >> match: aCharacter [

	aCharacter = lookahead
		ifFalse: [
			RxParser 
				signalSyntaxException: 'unexpected character: ', (String with: lookahead)
				at: source position].
	^source atEnd
		ifTrue: [lookahead := nil]
		ifFalse: [lookahead := source next]
]

{ #category : #accessing }
RxCharSetParser >> parse [

	lookahead = $- ifTrue:
		[self addChar: $-.
		self match: $-].
	[lookahead isNil] whileFalse: [self parseStep].
	^elements
]

{ #category : #parsing }
RxCharSetParser >> parseCharOrRange [

	| firstChar |

	firstChar := lookahead.
	self match: firstChar.
	lookahead = $-
		ifTrue: [ self match: $-.
			lookahead
				ifNil: [ ^ self
						addChar: firstChar;
						addChar: $-
					]
				ifNotNil: [ self addRangeFrom: firstChar to: lookahead.
					^ self match: lookahead
					]
			].
	self addChar: firstChar
]

{ #category : #parsing }
RxCharSetParser >> parseEscapeChar [

	self match: $\.
	$- = lookahead
		ifTrue: [elements add: (RxsCharacter with: $-)]
		ifFalse: [elements add: (RxsPredicate forEscapedLetter: lookahead)].
	self match: lookahead
]

{ #category : #parsing }
RxCharSetParser >> parseNamedSet [

	| name |
	self match: $[; match: $:.
	name := (String with: lookahead), (source upTo: $:).
	lookahead := source next.
	self match: $].
	elements add: (RxsPredicate forNamedClass: name)
]

{ #category : #parsing }
RxCharSetParser >> parseStep [

	lookahead = $[ ifTrue:
		[source peek = $:
			ifTrue: [^self parseNamedSet]
			ifFalse: [^self parseCharOrRange]].
	lookahead = $\ ifTrue:
		[^self parseEscapeChar].
	lookahead = $- ifTrue:
		[RxParser signalSyntaxException: 'invalid range' at: source position].
	self parseCharOrRange
]
