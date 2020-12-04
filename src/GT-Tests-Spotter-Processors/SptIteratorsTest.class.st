Class {
	#name : #SptIteratorsTest,
	#superclass : #TestCase,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptIteratorsTest >> testBeginsWithFilterDoesNotFetchTwiceIfTheFilterIsARefinementOfPreviousText [

	| it |
	it := (SptGeneratorBlockIterator on: [ :x | x value: 'bbb'; value:'bbb1'; error ]) beginsWithFilter: 'b'.
	it next.
	it beginsWithFilter: 'bb'.
	
	self assert: it next equals: 'bbb'
]

{ #category : #tests }
SptIteratorsTest >> testBeginsWithFilterReturnsValidResults [

	| it |
	it := (SptCollectionIterator on: { 'aaa'. 'bbb'. 'ccc' }) beginsWithFilter: 'b'.
	self assert: it next equals: 'bbb'
]

{ #category : #tests }
SptIteratorsTest >> testBeginsWithFilterReturnsValidResultsWithTheSameFilter [

	| it |
	it := (SptCollectionIterator on: { 'aaa'. 'bbb'. 'ccc' }) beginsWithFilter: 'b'.
	it beginsWithFilter: 'b'.
	
	self assert: it next equals: 'bbb'
]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectedCollectionIsFiltered [

	| it |
	it := ((SptCollectionIterator on: #(1 2 3)) 
		collect: [ :e | e * 2 ])
		select: [:e | e even].
	self assertCollection: (it next: 3) hasSameElements: #(2 4 6).

]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectionIsFiltered [

	| it |

	it := (SptCollectionIterator on: #(1 2 3)) select: [:e | e even].

	self assertCollection: (it next: 3) hasSameElements: #(2).

]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectionReturnsTheCollection [

	| it |
	it := SptCollectionIterator on: #(1 2 3).
	self assertCollection: (it next:3) hasSameElements: #(1 2 3)
]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectionReturnsTheFirstValue [

	| it |

	it := SptCollectionIterator on: #(1 2 3).
	self assert: (it next) equals: 1
]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectionReturnsTheFirstValueAndSecondValue [

	| it |

	it := SptCollectionIterator on: #(1 2 3).
	self assert: (it next) equals: 1.
	self assert: (it next) equals: 2	
]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectionWithCollectReturnsCorrectFirstValue [

	| it |
	it := (SptCollectionIterator on: #(1 2 3)) collect: [:e | e * 3].
	self assert: (it next) equals: 3.

]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectionWithCollectReturnsCorrectValues [

	| it |
	it := (SptCollectionIterator on: #(1 2 3)) collect: [:e | e * 3].
	self assertCollection: (it next: 3) hasSameElements: #(3 6 9).

]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectionWithTwoCollectReturnsCorrectFirstValue [

	| it |
	it := ((SptCollectionIterator on: #(1 2 3)) collect: [:e | e * 3]) collect: [:e | e * 2].
	self assert: it next equals: 6.

]

{ #category : #tests }
SptIteratorsTest >> testIteratorOnACollectionWithTwoCollectReturnsCorrectValues [

	| it |
	it := ((SptCollectionIterator on: #(1 2 3)) collect: [:e | e * 3]) collect: [:e | e * 2].
	self assertCollection: (it next: 3) hasSameElements: #(6 12 18).

]

{ #category : #tests }
SptIteratorsTest >> testNonDuplicatesRemoveDuplicates [

	| it |
	
	it := (SptCollectionIterator on: #(1 1)) asWithoutDuplicates.
	self assert: (it next:1) equals: #(1) asOrderedCollection
]

{ #category : #tests }
SptIteratorsTest >> testNonDuplicatesRemoveDuplicatesResetReturnsWithoutDuplicates [

	| it |
	
	it := (SptCollectionIterator on: #(1 1 2 2 3 3)) asWithoutDuplicates.
	self assert: (it next:3) equals: #(1 2 3) asOrderedCollection.
	
	it reset. 
	
	self assert: (it next:3) equals: #(1 2 3) asOrderedCollection 
]

{ #category : #tests }
SptIteratorsTest >> testNonDuplicatesRemoveDuplicatesWithThree [

	| it |
	
	it := (SptCollectionIterator on: #(1 1 2 2 3 3)) asWithoutDuplicates.
	self assert: (it next:3) equals: #(1 2 3) asOrderedCollection 
]

{ #category : #tests }
SptIteratorsTest >> testSequenceDetectsAtEnd [

	| it |
	it := (SptCollectionIterator on: #(1 2 3)) , (SptCollectionIterator on: #(4 5 6)).
	it next: 3.
	self deny: it atEnd.
	it next: 3.
	self assert: it atEnd.
		

	
]

{ #category : #tests }
SptIteratorsTest >> testSequenceReturnsTheSequenceOfAllResults [

	| it |
	it := (SptCollectionIterator on: #(1 2 3)) , (SptCollectionIterator on: #(4 5 6)).
	
	self assertCollection: (it next: 6) hasSameElements: #(1 2 3 4 5 6)
]

{ #category : #tests }
SptIteratorsTest >> testSequenceReturnsTheSequenceOfAllResultsInParts [

	| it |
	it := (SptCollectionIterator on: #(1 2 3)) , (SptCollectionIterator on: #(4 5 6)).
	
	self assertCollection: (it next: 3) hasSameElements: #(1 2 3).
	self assertCollection: (it next: 3) hasSameElements: #(4 5 6)	
]

{ #category : #tests }
SptIteratorsTest >> testSubstringFilterDoesNotFetchTwiceIfTheFilterIsARefinementOfPreviousText [

	| it |
	it := (SptGeneratorBlockIterator on: [ :x | x value: '1bbb'; value:'11bbb1'; error ]) substringFilter: 'b'.
	it next.
	it substringFilter: 'bb'.
	
	self assert: it next equals: '1bbb'
]

{ #category : #tests }
SptIteratorsTest >> testSubstringFilterReturnsValidResults [

	| it |
	it := (SptCollectionIterator on: { 'xxxaaa'. 'xxxbbb'. 'xxxccc' }) substringFilter: 'b'.
	self assert: it next equals: 'xxxbbb'
]

{ #category : #tests }
SptIteratorsTest >> testSubstringFilterReturnsValidResultsWithTheSameFilter [

	| it |
	it := (SptCollectionIterator on: { 'aaa'. 'xxxbbb'. 'ccc' }) substringFilter: 'b'.
	it substringFilter: 'b'.
	
	self assert: it next equals: 'xxxbbb'
]

{ #category : #tests }
SptIteratorsTest >> testWordsAwareIteratorFiltersByASequenceOfCamelCase [

	| inner it |
	
	inner := (SptCollectionIterator on: { 'aaa'. 'xxxyyybbb'. 'xxxcccbbb' }) asSubstringFilter.
	it := SptWordsAwareFilter new
		inner: inner;
		yourself.
		
	it filteringText: 'xxxBbb'.
	
	self assert: it next equals: 'xxxyyybbb'.
	self assert: it next equals: 'xxxcccbbb'	
]

{ #category : #tests }
SptIteratorsTest >> testWordsAwareIteratorFiltersByASequenceOfCamelCaseMixingCase [

	| inner it |
	
	inner := (SptCollectionIterator on: { 'aaa'. 'xxxyyyBBB'. 'XXXcccbbb'. 'xxxcccbbb' }) asSubstringFilter.
	it := SptWordsAwareFilter new
		inner: inner;
		yourself.
		
	it filteringText: 'xxxBbb'.
	
	self assert: it next equals: 'xxxyyyBBB'.
	self assert: it next equals: 'XXXcccbbb'.
	self assert: it next equals: 'xxxcccbbb'	
]

{ #category : #tests }
SptIteratorsTest >> testWordsAwareIteratorFiltersByASequenceOfCamelCaseWithUppercase [

	| inner it |
	
	inner := (SptCollectionIterator on: { 'aaa'. 'xxxyyybbb'. 'xxxcccbbb' }) asSubstringFilter.
	it := SptWordsAwareFilter new
		inner: inner;
		yourself.
		
	it filteringText: 'XxxBbb'.
	
	self assert: it next equals: 'xxxyyybbb'.
	self assert: it next equals: 'xxxcccbbb'	
]

{ #category : #tests }
SptIteratorsTest >> testWordsAwareIteratorFiltersByASequenceOfWords [

	| inner it |
	
	inner := (SptCollectionIterator on: { 'aaa'. 'xxxyyybbb'. 'xxxcccbbb' }) asSubstringFilter.
	it := SptWordsAwareFilter new
		inner: inner;
		yourself.
		
	it filteringText: 'xxx bbb'.
	
	self assert: it next equals: 'xxxyyybbb'.
	self assert: it next equals: 'xxxcccbbb'	
]

{ #category : #tests }
SptIteratorsTest >> testWordsAwareIteratorFiltersByASingleWord [

	| inner it |
	
	inner := (SptCollectionIterator on: { 'aaa'. 'xxxyyybbb'. 'xxxcccbbb' }) asSubstringFilter.
	it := SptWordsAwareFilter new
		inner: inner;
		yourself.
		
	it filteringText: 'xxx'.
	
	self assert: it next equals: 'xxxyyybbb'.
	self assert: it next equals: 'xxxcccbbb'	
]

{ #category : #tests }
SptIteratorsTest >> testWordsAwareIteratorFiltersByASingleWordFindingNothing [

	| inner it |
	
	inner := (SptCollectionIterator on: { 'aaa'. 'xxxyyybbb'. 'xxxcccbbb' }) asSubstringFilter.
	it := SptWordsAwareFilter new
		inner: inner;
		yourself.
		
	it filteringText: 'zzz'.
	
	self assert: it next equals: nil
]

{ #category : #tests }
SptIteratorsTest >> testWordsAwareIteratorFiltersLeftOutBecauseSecondWord [

	| inner it |
	
	inner := (SptCollectionIterator on: { 'aaa'. 'xxxyyybbb'. 'xxxcccbbb' }) asSubstringFilter.
	it := SptWordsAwareFilter new
		inner: inner;
		yourself.
		
	it filteringText: 'xxx aaa'.
	
	self assert: it next equals: nil.

]
