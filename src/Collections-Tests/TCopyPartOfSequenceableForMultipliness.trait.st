"
A trait for test purposes
"
Trait {
	#name : #TCopyPartOfSequenceableForMultipliness,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TCopyPartOfSequenceableForMultipliness >> collectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TCopyPartOfSequenceableForMultipliness >> test0FixtureCopyPartOfForMultipliness [

	self collectionWithSameAtEndAndBegining.
	self assert: self collectionWithSameAtEndAndBegining first = self collectionWithSameAtEndAndBegining last.
	self assert: self collectionWithSameAtEndAndBegining size > 1.
	1 to: self collectionWithSameAtEndAndBegining size do: [ :i | 
		i > 1 & (i < self collectionWithSameAtEndAndBegining size)
			ifTrue: [ self deny: (self collectionWithSameAtEndAndBegining at: i) = self collectionWithSameAtEndAndBegining first ] ]
]

{ #category : #'tests - copying part of sequenceable for multipliness' }
TCopyPartOfSequenceableForMultipliness >> testCopyAfterLastWithDuplicate [

	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.
	
	"collectionWithSameAtEndAndBegining first and last elements are equals.
	'copyAfter:' should copy after the last occurence of element :"
	result := collection copyAfterLast: (element ).
	
	"Verify content"
	self assert: result isEmpty


]

{ #category : #'tests - copying part of sequenceable for multipliness' }
TCopyPartOfSequenceableForMultipliness >> testCopyAfterWithDuplicate [

	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.
	
	"collectionWithSameAtEndAndBegining first and last elements are equals.
	'copyAfter:' should copy after the first occurence:"
	result := collection   copyAfter: (element ).
	
	"Verifying content"
	result withIndexDo: 
		[:e :i | self assert: (collection at: (i + 1 )) equals: (result at: (i)) ].

	"Verify size"
	self assert: result size equals: (collection size - 1).
]

{ #category : #'tests - copying part of sequenceable for multipliness' }
TCopyPartOfSequenceableForMultipliness >> testCopyUpToLastWithDuplicate [

	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.
	
	"collectionWithSameAtEndAndBegining first and last elements are equals.
	'copyUpToLast:' should copy until the last occurence :"
	result := collection copyUpToLast: (element ).
	
	"Verify content"
	result withIndexDo: 
		[:el :i | self assert: (result at: i) equals: (collection at: i) ].
	
	self assert: result size equals: (collection size - 1)


]

{ #category : #'tests - copying part of sequenceable for multipliness' }
TCopyPartOfSequenceableForMultipliness >> testCopyUpToWithDuplicate [

	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.
	
	"collectionWithSameAtEndAndBegining first and last elements are equals.
 	 'copyUpTo:' should copy until the first occurence"
	result := collection copyUpTo: (element ).
	
	"Verify content"
	self assert: result isEmpty


]
