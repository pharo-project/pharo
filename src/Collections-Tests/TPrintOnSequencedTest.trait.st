"
A trait for test purposes
"
Trait {
	#name : #TPrintOnSequencedTest,
	#traits : 'TPrintTest',
	#classTraits : 'TPrintTest classTrait',
	#category : #'Collections-Tests-Abstract'
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
	result:=''.
	aStream:= ReadWriteStream on: result.
	
	self nonEmpty printElementsOn: aStream .
	allElementsAsString:=(result findBetweenSubstrings: ' ' ).
	allElementsAsString withIndexDo: 
		[:el :i | self assert: (allElementsAsString at:i)=((self nonEmpty at:i)asString)].
]

{ #category : #'tests - printing' }
TPrintOnSequencedTest >> testPrintNameOn [

	| aStream result |
	result:=''.
	aStream:= ReadWriteStream on: result.
	
	self nonEmpty printNameOn: aStream .
	Transcript show: result asString.
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
	
	self nonEmpty printOn: aStream delimiter: ', ' .
	
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
	
	allElementsAsString:=(result findBetweenSubstrings: ', ' ).
	allElementsAsString withIndexDo: 
		[:el :i | 
		i<(allElementsAsString size-1 )
			ifTrue: [self assert: (allElementsAsString at:i) equals: ((self nonEmpty at:i)asString)].
		i=(allElementsAsString size-1)
			ifTrue:[ self deny: (allElementsAsString at:i) equals: ('and')asString].
		i=(allElementsAsString size)
			ifTrue: [self assert: (allElementsAsString at:i) equals: ((self nonEmpty at:(i-1))asString)]]
]

{ #category : #'tests - printing' }
TPrintOnSequencedTest >> testStoreOn [
" for the moment work only for collection that include simple elements such that Integer"

"| string str result cuttedResult index elementsAsStringExpected elementsAsStringObtained tmp |
string := ''.
str := ReadWriteStream  on: string.
elementsAsStringExpected := OrderedCollection new.
elementsAsStringObtained := OrderedCollection new.
self nonEmpty do: [ :each | elementsAsStringExpected  add: each asString].

self nonEmpty storeOn: str.
result := str contents .
cuttedResult := ( result findBetweenSubstrings: ';' ).

index := 1.

cuttedResult do:
	[ :each |
	index = 1 
		ifTrue: [ self assert: (each beginsWith: ( tmp := '((' , self nonEmpty class asString , ' new) add: '           )).
				tmp := each copyFrom: ( tmp size + 1) to: ( each size ).
				elementsAsStringObtained add: tmp.
				index := index + 1. ]
		ifFalse:  [ 
		 index < cuttedResult size
			ifTrue:[self assert: (each beginsWith: ( tmp:=  ' add: '   )).
				tmp := each copyFrom: ( tmp size + 1) to: ( each size ).
				elementsAsStringObtained add: tmp.
					index := index + 1.]
			ifFalse: [self assert: ( each = ' yourself)' ) ].
			]
	
	].


	elementsAsStringExpected do: [ :each | self assert: (elementsAsStringExpected occurrencesOf: each ) = ( elementsAsStringObtained occurrencesOf: each) ]"


]
