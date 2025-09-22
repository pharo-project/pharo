"
A trait for test purposes
"
Trait {
	#name : 'TAsStringOnDelimiterTest',
	#category : 'Collections-Abstract-Tests-Traits',
	#package : 'Collections-Abstract-Tests',
	#tag : 'Traits'
}

{ #category : 'requirements' }
TAsStringOnDelimiterTest >> empty [

	^ self explicitRequirement
]

{ #category : 'requirements' }
TAsStringOnDelimiterTest >> nonEmpty [

	^ self explicitRequirement
]

{ #category : 'requirements' }
TAsStringOnDelimiterTest >> nonEmpty1Element [
" return a collection of size 1 including one element"
	^ self explicitRequirement
]

{ #category : 'tests - fixture' }
TAsStringOnDelimiterTest >> test0FixtureAsStringCommaAndDelimiterTest [
	self nonEmpty.
	self denyEmpty: self nonEmpty.
	self empty.
	self assertEmpty: self empty.
	self nonEmpty1Element.
	self assert: self nonEmpty1Element size = 1
]

{ #category : 'tests - as string comma delimiter sequenceable' }
TAsStringOnDelimiterTest >> testAsStringOnDelimiterEmpty [
	self assert: (String streamContents: [ :emptyStream | self empty asStringOn: emptyStream delimiter: ', ' ]) equals: ''
]

{ #category : 'tests - as string comma delimiter sequenceable' }
TAsStringOnDelimiterTest >> testAsStringOnDelimiterLastEmpty [
	self assert: (String streamContents: [ :emptyStream | self empty asStringOn: emptyStream delimiter: ', ' last: 'and' ]) equals: ''
]

{ #category : 'tests - as string comma delimiter sequenceable' }
TAsStringOnDelimiterTest >> testAsStringOnDelimiterLastMore [

	| delim multiItemStream result last allElementsAsString |

	delim := ', '.
	last := ' and '.
	result:=''.
	multiItemStream := ReadWriteStream on:result.
	self nonEmpty asStringOn: multiItemStream delimiter: delim last: last.
	result := multiItemStream contents.
	allElementsAsString:=(result findBetweenSubstrings: delim ).
	self nonEmpty do:[:each |
		self assert: (self nonEmpty occurrencesOf: each) = (allElementsAsString occurrencesOf: each asString)].
	self assert: ((allElementsAsString at: (allElementsAsString size - 1))=('and'))
]

{ #category : 'tests - as string comma delimiter sequenceable' }
TAsStringOnDelimiterTest >> testAsStringOnDelimiterLastOne [

	| delim oneItemStream result |

	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream  do:
		[:each1 |
		self nonEmpty1Element do: [:each2 |self assert: each1 = (each2 asString) ]
		 ]
]

{ #category : 'tests - as string comma delimiter sequenceable' }
TAsStringOnDelimiterTest >> testAsStringOnDelimiterMore [

	| delim multiItemStream result allElementsAsString |

	delim := ', '.
	result:= ''.
	multiItemStream := ReadWriteStream on:result.
	self nonEmpty asStringOn: multiItemStream delimiter: delim.
	result := multiItemStream contents.
	allElementsAsString := (result findBetweenSubstrings: delim ).
	self nonEmpty do:
		[:each |
		self assert: (self nonEmpty occurrencesOf: each)=(allElementsAsString occurrencesOf: each asString).
		]
]

{ #category : 'tests - as string comma delimiter sequenceable' }
TAsStringOnDelimiterTest >> testAsStringOnDelimiterOne [

	| delim oneItemStream result |

	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim.
	oneItemStream  do:
		[:each1 |
		self nonEmpty1Element do: [:each2 |self assert: each1 = (each2 asString) ]
		 ]
]
