"
STONReaderTests test materialization.

"
Class {
	#name : #STONReaderTests,
	#superclass : #TestCase,
	#category : #'STON-Tests'
}

{ #category : #private }
STONReaderTests >> materialize: string [
	^ STON reader 
		on: string readStream;
		next
]

{ #category : #tests }
STONReaderTests >> testAssociation [
	self assert: (self materialize: '''foo'':1') = ('foo' -> 1).
	self assert: (self materialize: '#bar:2') = (#bar -> 2).
	self assert: (self materialize: '''foo bar'':#ok') = ('foo bar' -> #ok).
	self assert: (self materialize: '123:456') = (123 -> 456).
	
	self assert: (self materialize: '''foo'' : 1') = ('foo' -> 1).
	self assert: (self materialize: '#bar : 2') = (#bar -> 2).
	self assert: (self materialize: '''foo bar'' : #ok') = ('foo bar' -> #ok).
	self assert: (self materialize: '123 : -456') = (123 -> -456).
	
	self assert: (self materialize: '#foo : 1 : 2') = (#foo -> (1 -> 2))

]

{ #category : #tests }
STONReaderTests >> testBoolean [
	self assert: (self materialize: 'true') equals: true.
	self assert: (self materialize: 'false') equals: false
]

{ #category : #tests }
STONReaderTests >> testByteArray [
	self assert: (self materialize: 'ByteArray[''010203'']') = #(1 2 3) asByteArray

]

{ #category : #tests }
STONReaderTests >> testCharacter [
	self assert: (self materialize: 'Character[''A'']') == $A.
]

{ #category : #tests }
STONReaderTests >> testClass [
	self assert: (self materialize: 'Class[#Point]') equals: Point
]

{ #category : #tests }
STONReaderTests >> testClassWithUnderscore [

	| cls data reader |

	cls := Object newAnonymousSubclass.
	cls setName: #A_B_C123AnonClass.

	data := STON toString: cls new.
	reader := STONReader on: data readStream.

	(reader instVarNamed: #classes) 
		at: cls name 
		put: cls.

	self assert: reader next class equals: cls
]

{ #category : #tests }
STONReaderTests >> testConvertingNewLines [
	| input result output |
	input := '''line ending with CR', String cr, 
		'line ending with LF', String lf, 
		'line ending with CRLF', String crlf, ''''.
	output := 'line ending with CR', String crlf, 
		'line ending with LF', String crlf, 
		'line ending with CRLF', String crlf.
	result := (STON reader on: input readStream) newLine: String crlf; convertNewLines: true; next.
	self assert: result equals: output.
	output := 'line ending with CR', String cr, 
		'line ending with LF', String cr, 
		'line ending with CRLF', String cr.
	result := (STON reader on: input readStream) newLine: String cr; convertNewLines: true; next.
	self assert: result equals: output
]

{ #category : #tests }
STONReaderTests >> testDate [
	| date |
	date := Date year: 2012 month: 1 day: 1.
	self assert: (self materialize: 'Date[''2012-01-01'']') = date
]

{ #category : #tests }
STONReaderTests >> testDateAndTime [
	| dateAndTime |
	dateAndTime := DateAndTime year: 2012 month: 1 day: 1 hour: 6 minute: 30 second: 15 offset: 1 hour.
	self assert: (self materialize: 'DateAndTime[''2012-01-01T06:30:15+01:00'']') = dateAndTime
]

{ #category : #tests }
STONReaderTests >> testDeepStructure [
	| holder deepest structure writer ston reader result |
	"Create a deep nested structure so that the deepest element is a reference back to a top level holder."
	holder := Array with: 42.
	deepest := Array with: holder.
	structure := deepest.
	1 * 1024 timesRepeat: [ structure := Array with: structure ].
	structure := Array with: holder with: structure.
	writer := STON writer optimizeForLargeStructures.
	ston := String streamContents: [ :out | (writer on: out) nextPut: structure ].
	"After reading, the second pass will have to go down the structure to resolve the reference."
	reader := STON reader optimizeForLargeStructures.
	result := (reader on: ston readStream) next.
	self assert: result equals: structure
]

{ #category : #tests }
STONReaderTests >> testDictionary [
	| collection |
	collection := STON mapClass new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self materialize: '{1:1,2:2}') = collection.
	self assert: (self materialize: '{}') = STON mapClass new.

]

{ #category : #tests }
STONReaderTests >> testDictionaryWithComplexKeys [
	| collection reader |
	collection := STON mapClass new at: true put: 1; at: #(foo) put: 2; yourself.
	"allowing complex map keys used to be optional, now it is always the default"
	reader := STONReader on: '{true:1,[#foo]:2}' readStream.
	self assert: reader next = collection
]

{ #category : #tests }
STONReaderTests >> testDictionaryWithIndirectReferenceKeys [
	| keysCollection dictionary ston object |
	keysCollection := OrderedCollection streamContents: [ :out |
		10 timesRepeat: [ out nextPut: UUID new ] ].
	dictionary := Dictionary new.
	keysCollection doWithIndex: [ :each :index | 
		dictionary at: (Array with: each) put: index ].
	object := Array with: keysCollection with: dictionary.
	ston := STON toStringPretty: object.
	object := (STON reader on: ston readStream) next.
	object first doWithIndex: [ :each :index |
		self assert: (object second at: (Array with: each)) equals: index ].
	self assert: object second isHealthy 
]

{ #category : #tests }
STONReaderTests >> testDictionaryWithReferenceKeys [
	| keysCollection dictionary ston object |
	keysCollection := OrderedCollection streamContents: [ :out |
		10 timesRepeat: [ out nextPut: UUID new ] ].
	dictionary := Dictionary new.
	keysCollection doWithIndex: [ :each :index | 
		dictionary at: each put: index ].
	object := Array with: keysCollection with: dictionary.
	ston := STON toStringPretty: object.
	object := (STON reader on: ston readStream) next.
	object first doWithIndex: [ :each :index |
		self assert: (object second at: each) equals: index ].
	self assert: object second isHealthy 
]

{ #category : #tests }
STONReaderTests >> testError [
	#( 'foo' '{foo:}' '{foo,}' '[1,]' '+1' ']' '#' '' '  ' '	' 'nul' 'tru' 'fals' ) do: [ :each |
		self 
			should: [ self materialize: each ] 
			raise: STONReaderError ]

]

{ #category : #tests }
STONReaderTests >> testFloat [
	self assert: ((self materialize: '1.5') closeTo: 1.5).
	self assert: ((self materialize: '-1.5') closeTo: -1.5).
	self assert: (self materialize: '0.0') isZero.
	self assert: (Float pi closeTo: (self materialize: '3.14149')).
	self assert: (1/3 closeTo: (self materialize: '0.333333')).
	self assert: ((self materialize: '1.0e100') closeTo: (10 raisedTo: 100)).
	self assert: ((self materialize: '1.0e-100') closeTo: (10 raisedTo: -100)).
	self assert: ((self materialize: '-1.0e-100') closeTo: (10 raisedTo: -100) negated)
]

{ #category : #tests }
STONReaderTests >> testIdentityDictionary [
	| collection |
	collection := IdentityDictionary new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self materialize: 'IdentityDictionary{1:1,2:2}') = collection.
	self assert: (self materialize: 'IdentityDictionary{}') = IdentityDictionary new.

]

{ #category : #tests }
STONReaderTests >> testIllegalCharacterEscapes [
	self should: [ STON fromString: '''\a''' ] raise: STONReaderError.
	self should: [ STON fromString: '''\u''' ] raise: STONReaderError.
	self should: [ STON fromString: '''\u00''' ] raise: STONReaderError.
	self should: [ STON fromString: '''\u000''' ] raise: STONReaderError.
	self should: [ STON fromString: '''\*''' ] raise: STONReaderError
]

{ #category : #tests }
STONReaderTests >> testInteger [
	self assert: (self materialize: '1') = 1.
	self assert: (self materialize: '-1') = -1.
	self assert: (self materialize: '0') = 0.
	self assert: (self materialize: '1234567890') = 1234567890.
	self assert: (self materialize: '-1234567890') = -1234567890
]

{ #category : #tests }
STONReaderTests >> testJsonString [
	"Allow double quotes for backwards JSON compatibility"
	
	| string |
	self assert: (self materialize: '"foo"') = 'foo'.
	self assert: (self materialize: '"FOO"') = 'FOO'.
	self assert: (self materialize: '"\u00E9l\u00E8ve en Fran\u00E7ais"') = 'élève en Français'.
	string := String withAll: { 
		$". $'. $\. Character tab. Character cr. Character lf.  Character newPage. Character backspace }.
	self assert: (self materialize: '"\"\''\\\t\r\n\f\b"') = string.

]

{ #category : #tests }
STONReaderTests >> testList [
	self assert: STON listClass = Array.
	self assert: (self materialize: '[1,2,3]') = (STON listClass with: 1 with: 2 with: 3).
	self assert: (self materialize: '[]') = STON listClass new
]

{ #category : #tests }
STONReaderTests >> testMap [
	self assert: (self materialize: '{#foo:1}') = (STON mapClass new at: #foo put: 1; yourself).
	self assert: (self materialize: '{}') = STON mapClass new
]

{ #category : #tests }
STONReaderTests >> testMetaclass [
	self assert: (self materialize: 'Metaclass[#Point]') equals: Point class
]

{ #category : #tests }
STONReaderTests >> testMultiple [
	| reader |
	reader := STON reader 
		on: '123 -123 nil #foo true [ 0 ] false { #one : 1 }' readStream.
	self deny: reader atEnd.
	self assert: reader next equals: 123. 
	self assert: reader next equals: -123. 
	self assert: reader next equals: nil. 
	self assert: reader next equals: #foo. 
	self assert: reader next equals: true. 
	self assert: reader next equals: { 0 }. 
	self assert: reader next equals: false. 
	self assert: reader next equals: (Dictionary with: #one -> 1). 
	self assert: reader atEnd.

]

{ #category : #tests }
STONReaderTests >> testNewSymbol [
	| n notASymbol shouldBeSymbol |
	
	"Find a name that has not yet been interned"
	n := 0.
	[ Symbol hasInterned: (notASymbol := 'notASymbol', n printString) ifTrue: [ :symbol | symbol ] ] 
		whileTrue: [ n := n + 1 ].
	"Parsing the new, not yet interned name should create a new Symbol"
	shouldBeSymbol := self materialize: '#', notASymbol.
	self assert: (shouldBeSymbol isSymbol and: [ notASymbol = shouldBeSymbol asString ])

]

{ #category : #tests }
STONReaderTests >> testNil [
	self assert: (self materialize: 'nil') isNil
]

{ #category : #tests }
STONReaderTests >> testNonBMPCharacterDecoding [
	"Characters not in the Basic Multilingual Plane are encoded as a UTF-16 surrogate pair"

	| string object |
	string := String with: 16r1D11E asCharacter. "MUSICAL SYMBOL G CLEF"
	object := (STON fromString: '''\uD834\uDD1E''').
	self assert: object equals: string
]

{ #category : #tests }
STONReaderTests >> testNull [
	self assert: (self materialize: 'null') isNil
]

{ #category : #tests }
STONReaderTests >> testObject [
	self assert: (self materialize: 'Point[1,2]') = (1@2).
	self assert: (self materialize: 'Point[1.5,-0.5]') = (1.5 @ -0.5).
]

{ #category : #tests }
STONReaderTests >> testOrderedCollection [
	| collection |
	collection := OrderedCollection with: 1 with: 2 with: 3.
	self assert: (self materialize: 'OrderedCollection[1,2,3]') = collection.
	self assert: (self materialize: 'OrderedCollection[]') = OrderedCollection new.
]

{ #category : #tests }
STONReaderTests >> testPoint [
	self assert: (self materialize: 'Point[1,2]') = (1@2)
]

{ #category : #tests }
STONReaderTests >> testReferenceCycle [
	| array |
	array := (self materialize: '[1,@1]').
	self assert: array class = STON listClass.
	self assert: array size = 2.
	self assert: array first = 1.
	self assert: array second == array
]

{ #category : #tests }
STONReaderTests >> testReferenceSharing [
	| one array |
	one := { #one }.
	array := (self materialize: '[[#one],@2,@2]').
	self assert: array = (STON listClass with: one with: one with: one).
	self assert: array first == array second.
	self assert: array first == array third
]

{ #category : #tests }
STONReaderTests >> testSetWithIndirectReferenceElements [
	| elementsCollection set ston object |
	elementsCollection := OrderedCollection streamContents: [ :out |
		10 timesRepeat: [ out nextPut: UUID new ] ].
	set := Set withAll: (elementsCollection collect: [ :each | Array with: each ]).
	object := Array with: elementsCollection with: set.
	ston := STON toStringPretty: object.
	object := STON fromString: ston readStream.
	object first do: [ :each |
		self assert: (object second includes: (Array with: each)) ].
	self assert: object second isHealthy 
]

{ #category : #tests }
STONReaderTests >> testSetWithReferenceElements [
	| elementsCollection set ston object |
	elementsCollection := OrderedCollection streamContents: [ :out |
		10 timesRepeat: [ out nextPut: UUID new ] ].
	set := Set withAll: elementsCollection.
	object := Array with: elementsCollection with: set.
	ston := STON toStringPretty: object.
	object := STON fromString: ston readStream.
	object first do: [ :each |
		self assert: (object second includes: each) ].
	self assert: object second isHealthy 
]

{ #category : #tests }
STONReaderTests >> testStreaming [
	| reader |
	reader := STON reader 
		on: '1 2 3 4 5 6 7 8 9 10' readStream.
	self 
		assert: (Array streamContents: [ :stream |
			[ reader atEnd] whileFalse: [ 
				stream nextPut: reader next ] ]) sum
		equals: #(1 2 3 4 5 6 7 8 9 10) sum
]

{ #category : #tests }
STONReaderTests >> testString [
	| string |
	self assert: (self materialize: '''foo''') = 'foo'.
	self assert: (self materialize: '''FOO''') = 'FOO'.
	self assert: (self materialize: '''\u00E9l\u00E8ve en Fran\u00E7ais''') = 'élève en Français'.
	string := String withAll: { 
		$". $'. $\. $/. Character tab. Character cr. Character lf.  Character newPage. Character backspace }.
	self assert: (self materialize: '''\"\''\\\/\t\r\n\f\b''') = string.

]

{ #category : #tests }
STONReaderTests >> testSymbol [
	self assert: (self materialize: '#''foo''') = #foo.
	self assert: (self materialize: '#foo') = #foo
]

{ #category : #tests }
STONReaderTests >> testTime [
	| time |
	time := Time hour: 6 minute: 30 second: 15.
	self assert: (self materialize: 'Time[''06:30:15'']') equals: time.
	time := Time hour: 6 minute: 30 second: 15 nanoSecond: 123.
	self assert: (self materialize: 'Time[''06:30:15.000000123'']') equals: time.

]

{ #category : #tests }
STONReaderTests >> testUnknownClasses [
	| input object |
	input := 'FooBar { #foo : 1, #bar : true }'.
	self should: [ self materialize: input ] raise: STONReaderError.
	object := STON reader 
		acceptUnknownClasses: true; 
		on: input readStream; 
		next.
	self assert: object class equals: STON mapClass.
	self assert: (object at: #foo) equals: 1.
	self assert: (object at: #bar).
	self assert: (object at: STON classNameKey) equals: #FooBar
]

{ #category : #tests }
STONReaderTests >> testUser [
	| user |
	(user := STONTestUser new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (self materialize: 'STONTestUser{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user
]

{ #category : #tests }
STONReaderTests >> testUser2 [
	| user |
	(user := STONTestUser2 new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (self materialize: 'STONTestUser2{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user
]

{ #category : #tests }
STONReaderTests >> testWhitespace [
	| whitespace |
	whitespace := { Character space. Character tab. Character cr. Character lf }.
	self assert: (self materialize: whitespace, '123') = 123
	
]

{ #category : #tests }
STONReaderTests >> testWideSymbol [
	self assert: (self materialize: '#''яблоко''') = #яблоко.
	self assert: (self materialize: '#яблоко') = #яблоко
]
