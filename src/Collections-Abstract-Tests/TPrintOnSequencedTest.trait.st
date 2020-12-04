"
A trait for test purposes
"
Trait {
	#name : #TPrintOnSequencedTest,
	#traits : 'TPrintTest',
	#classTraits : 'TPrintTest classTrait',
	#category : #'Collections-Abstract-Tests-Traits'
}

{ #category : #requirements }
TPrintOnSequencedTest >> nonEmpty [
 self explicitRequirement
]

{ #category : #'tests - fixture' }
TPrintOnSequencedTest >> test0FixturePrintTest [

	self nonEmpty
]

{ #category : #'tests - printing' }
TPrintOnSequencedTest >> testPrintElementsOn [

	| aStream result allElementsAsString |
	result := ''.
	aStream := ReadWriteStream on: result.
	
	self nonEmpty printElementsOn: aStream.
	allElementsAsString:=(result findBetweenSubstrings: ' ').
	allElementsAsString withIndexDo: 
		[:el :i | self assert: el equals: ((self nonEmpty at: i)asString) ]
]

{ #category : #'tests - printing' }
TPrintOnSequencedTest >> testPrintNameOn [

	| aStream result |
	result := ''.
	aStream := ReadWriteStream on: result.
	
	self nonEmpty printNameOn: aStream.

	self nonEmpty class name first isVowel
		ifTrue:[ self assert: aStream contents equals: ('an ',self nonEmpty class name ) ]
		ifFalse:[ self assert: aStream contents equals: ('a ',self nonEmpty class name)].
]

{ #category : #'tests - printing' }
TPrintOnSequencedTest >> testPrintOn [

	| aStream result allElementsAsString |
	result := ''.
	aStream := ReadWriteStream on: result.
	
	self nonEmpty printOn: aStream .
	allElementsAsString:=(result findBetweenSubstrings: ' ' ).
	allElementsAsString withIndexDo: 
		[:el :i | 
		i=1
			ifTrue:[
				self accessCollection class name first isVowel 
					ifTrue:[self assert: el equals: 'an' ]
					ifFalse:[self assert: el equals: 'a'].].
		i=2
			ifTrue:[self assert: el equals: self accessCollection class name].
		i>2
			ifTrue:[self assert: (allElementsAsString at:i)=((self nonEmpty at:i)asString)]]
]

{ #category : #'tests - printing' }
TPrintOnSequencedTest >> testPrintOnDelimiter [

	| aStream result allElementsAsString |
	result := ''.
	aStream := ReadWriteStream on: result.
	
	self nonEmpty printOn: aStream delimiter: ', '.
	
	allElementsAsString := (result findBetweenSubstrings: ', ' ).
	allElementsAsString withIndexDo: 
		[:el :i |  self assert: el equals: ((self nonEmpty at:i)asString)]
]

{ #category : #'tests - printing' }
TPrintOnSequencedTest >> testPrintOnDelimiterLast [

	| aStream result allElementsAsString |
	result:=''.
	aStream:= ReadWriteStream on: result.
	
	self nonEmpty printOn: aStream delimiter: ', ' last: 'and'.
	
	allElementsAsString:=(result findBetweenSubstrings: ', ').
	allElementsAsString withIndexDo: 
		[:el :i | 
		i<(allElementsAsString size-1 )
			ifTrue: [self assert: (allElementsAsString at:i) equals: ((self nonEmpty at:i)asString)].
		i=(allElementsAsString size-1)
			ifTrue:[ self deny: (allElementsAsString at:i) equals: ('and')asString].
		i=(allElementsAsString size)
			ifTrue: [self assert: (allElementsAsString at:i) equals: ((self nonEmpty at:(i-1))asString)]]
]
