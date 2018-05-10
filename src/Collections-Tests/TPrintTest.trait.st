"
A trait for test purposes
"
Trait {
	#name : #TPrintTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TPrintTest >> nonEmpty [
 self explicitRequirement
]

{ #category : #'tests - fixture' }
TPrintTest >> test0FixturePrintTest [
	self nonEmpty.
	self deny: self nonEmpty isEmpty
]

{ #category : #'tests - printing' }
TPrintTest >> testPrintElementsOn [

	| aStream result allElementsAsString tmp |
	result:=''.
	aStream:= ReadWriteStream on: result.
	tmp:= OrderedCollection new.
	self nonEmpty do: [:each | tmp add: each asString].
	
	self nonEmpty printElementsOn: aStream .
	allElementsAsString:=(result findBetweenSubstrings: ' ' ).
	1 to: allElementsAsString size do:
		[:i | 
		self assert: (tmp occurrencesOf:(allElementsAsString at:i))=(allElementsAsString  occurrencesOf:(allElementsAsString at:i)).
			].
]

{ #category : #'tests - printing' }
TPrintTest >> testPrintNameOn [

	| aStream result |
	result:=''.
	aStream:= ReadWriteStream on: result.
	self nonEmpty printNameOn: aStream.
	self nonEmpty class name first isVowel
		ifTrue:[ self assert: aStream contents =('an ',self nonEmpty class name ) ]
		ifFalse:[self assert: aStream contents =('a ',self nonEmpty class name)].
]

{ #category : #'tests - printing' }
TPrintTest >> testPrintOn [
	| aStream result allElementsAsString tmp |
	result:=''.
	aStream:= ReadWriteStream on: result.
	tmp:= OrderedCollection new.
	self nonEmpty do: [:each | tmp add: each asString].
	
	self nonEmpty printOn: aStream .
	allElementsAsString:=(result findBetweenSubstrings: ' ' ).
	1 to: allElementsAsString size do:
		[:i | 
		i=1
			ifTrue:[
			self accessCollection class name first isVowel 
				ifTrue:[self assert: (allElementsAsString at:i)='an' ]
				ifFalse:[self assert: (allElementsAsString at:i)='a'].].
		i=2
			ifTrue:[self assert: (allElementsAsString at:i)=self accessCollection class name].
		i>2
			ifTrue:[self assert: (tmp occurrencesOf:(allElementsAsString at:i))=(allElementsAsString  occurrencesOf:(allElementsAsString at:i)).].	
			].
]

{ #category : #'tests - printing' }
TPrintTest >> testPrintOnDelimiter [
	
	| result allElementsAsString tmp |
	
	result := String streamContents: [ :aStream |
		tmp := OrderedCollection new.
		self nonEmpty do: [:each | tmp add: each asString].
		self nonEmpty printOn: aStream delimiter: ', ' ].
	
	allElementsAsString := (result findBetweenSubstrings: ', ').
	
	1 to: allElementsAsString size do:
		[:i | 
		self assert: (tmp occurrencesOf:(allElementsAsString at:i)) equals: (allElementsAsString  occurrencesOf:(allElementsAsString at:i)) ].
]

{ #category : #'tests - printing' }
TPrintTest >> testPrintOnDelimiterLast [

	| aStream result allElementsAsString tmp |
	result:=''.
	aStream:= ReadWriteStream on: result.
	tmp:= OrderedCollection new.
	self nonEmpty do: [:each | tmp add: each asString].
	
	self nonEmpty printOn: aStream delimiter: ', ' last: 'and'.
	
	allElementsAsString:=(result findBetweenSubstrings: ', ' ).
	1 to: allElementsAsString size do:
		[:i | 
		i<(allElementsAsString size-1 )
			ifTrue: [self assert: (tmp occurrencesOf: (allElementsAsString at:i))=(allElementsAsString  occurrencesOf: (allElementsAsString at:i))].
		i=(allElementsAsString size-1)
			ifTrue:[ self deny: (allElementsAsString at:i)=('and')asString].
		i=(allElementsAsString size)
			ifTrue: [self assert: (tmp occurrencesOf: (allElementsAsString at:i))=(allElementsAsString  occurrencesOf: (allElementsAsString at:i))].
			].
]

{ #category : #'tests - printing' }
TPrintTest >> testStoreOn [
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
