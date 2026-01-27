"
A trait for test purposes
"
Trait {
	#name : 'TAsStringOnDelimiterSequenceableTest',
	#traits : 'TAsStringOnDelimiterTest',
	#classTraits : 'TAsStringOnDelimiterTest classTrait',
	#category : 'Collections-Abstract-Tests-Traits',
	#package : 'Collections-Abstract-Tests',
	#tag : 'Traits'
}

{ #category : 'requirements' }
TAsStringOnDelimiterSequenceableTest >> empty [

	^ self explicitRequirement
]

{ #category : 'requirements' }
TAsStringOnDelimiterSequenceableTest >> nonEmpty [

	^ self explicitRequirement
]

{ #category : 'requirements' }
TAsStringOnDelimiterSequenceableTest >> nonEmpty1Element [
" return a collection of size 1 including one element"
	^ self explicitRequirement
]

{ #category : 'tests - fixture' }
TAsStringOnDelimiterSequenceableTest >> test0FixtureAsStringCommaAndDelimiterTest [
	self nonEmpty.
	self denyEmpty: self nonEmpty.
	self empty.
	self assertEmpty: self empty.
	self nonEmpty1Element.
	self assert: self nonEmpty1Element size = 1
]

{ #category : 'tests - as string comma delimiter sequenceable' }
TAsStringOnDelimiterSequenceableTest >> testAsStringOnDelimiterEmpty [

	self assert: (String streamContents: [ :emptyStream | self empty asStringOn: emptyStream delimiter: ', ' ]) equals: ''
]

{ #category : 'tests - as string comma delimiter sequenceable' }
TAsStringOnDelimiterSequenceableTest >> testAsStringOnDelimiterLastEmpty [

	self assert: (String streamContents: [ :emptyStream | self empty asStringOn: emptyStream delimiter: ', ' last: 'and' ]) equals: ''
]

{ #category : 'tests - comma and delimiter' }
TAsStringOnDelimiterSequenceableTest >> testAsStringOnDelimiterLastMore [

	| delim multiItemStream result last allElementsAsString |

	delim := ', '.
	last := ' and '.
	result:=''.
	multiItemStream := ReadWriteStream on:result.
	self nonEmpty  asStringOn: multiItemStream delimiter: delim last: last.
	result := multiItemStream contents.
	allElementsAsString:=(result findBetweenSubstrings: delim ).
	1 to: allElementsAsString size do:
		[:i |
		i<(allElementsAsString size-1 )
			ifTrue: [self assert: (allElementsAsString at:i)=((self nonEmpty at:i)asString)].
		i=(allElementsAsString size-1)
			ifTrue:[ self deny: (allElementsAsString at:i)=(last)asString].
		i=(allElementsAsString size)
			ifTrue: [self assert: (allElementsAsString at:i)=((self nonEmpty at:(i-1))asString)]]
]

{ #category : 'tests - comma and delimiter' }
TAsStringOnDelimiterSequenceableTest >> testAsStringOnDelimiterLastOne [

	| delim oneItemStream result |
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]

{ #category : 'tests - comma and delimiter' }
TAsStringOnDelimiterSequenceableTest >> testAsStringOnDelimiterMore [

	| delim multiItemStream result index |
	delim := ', '.
	result := ''.
	multiItemStream := ReadWriteStream on: result.
	self nonEmpty  asStringOn: multiItemStream delimiter: delim.
	result := multiItemStream contents.
	index:=1.
	(result findBetweenSubstrings: delim)do:
		[:each |
			self assert: each= ((self nonEmpty at:index) asString).
			index:=index+1 ]
]

{ #category : 'tests - comma and delimiter' }
TAsStringOnDelimiterSequenceableTest >> testAsStringOnDelimiterOne [

	| delim oneItemStream result |
	delim := ', '.
	result := ''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]
