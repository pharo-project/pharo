"
These test cases demonstrate addition of items into an OrderedCollection as well as item removal.

Some of the assertions are quite complicated and use a lot of collection protocol. Such methods do not test one single method, but protocol in general.
"
Class {
	#name : #OrderedCollectionTest,
	#superclass : #CollectionRootTest,
	#traits : 'TSortTest + TEmptySequenceableTest + TAddTest + TSequencedElementAccessTest + TIncludesWithIdentityCheckTest + TCloneTest + TSetArithmetic + TRemoveForMultiplenessTest + TCreationWithTest + TCopyTest + TPutBasicTest + TIterateSequencedReadableTest + TSubCollectionAccess + TIndexAccess + TCopySequenceableWithReplacement + TCopyPartOfSequenceable + TCopySequenceableSameContents + (TCopySequenceableWithOrWithoutSpecificElements - {#testForceToPaddingWith. #testForceToPaddingStartWith}) + TPrintOnSequencedTest + TAsStringCommaAndDelimiterSequenceableTest + TConvertTest + TConvertAsSetForMultiplinessIdentityTest + TSequencedConcatenationTest + TBeginsEndsWith + TReplacementSequencedTest + TIndexAccessForMultipliness + TCopyPartOfSequenceableForMultipliness + TConvertAsSortedTest + TPutTest + TSequencedStructuralEqualityTest + TOccurrencesForMultiplinessTest + TEnumeratingTest',
	#classTraits : 'TSortTest classTrait + TEmptySequenceableTest classTrait + TAddTest classTrait + TSequencedElementAccessTest classTrait + TIncludesWithIdentityCheckTest classTrait + TCloneTest classTrait + TSetArithmetic classTrait + TRemoveForMultiplenessTest classTrait + TCreationWithTest classTrait + TCopyTest classTrait + TPutBasicTest classTrait + TIterateSequencedReadableTest classTrait + TSubCollectionAccess classTrait + TIndexAccess classTrait + TCopySequenceableWithReplacement classTrait + TCopyPartOfSequenceable classTrait + TCopySequenceableSameContents classTrait + TCopySequenceableWithOrWithoutSpecificElements classTrait + TPrintOnSequencedTest classTrait + TAsStringCommaAndDelimiterSequenceableTest classTrait + TConvertTest classTrait + TConvertAsSetForMultiplinessIdentityTest classTrait + TSequencedConcatenationTest classTrait + TBeginsEndsWith classTrait + TReplacementSequencedTest classTrait + TIndexAccessForMultipliness classTrait + TCopyPartOfSequenceableForMultipliness classTrait + TConvertAsSortedTest classTrait + TPutTest classTrait + TSequencedStructuralEqualityTest classTrait + TOccurrencesForMultiplinessTest classTrait + TEnumeratingTest classTrait',
	#instVars : [
		'empty',
		'nonEmpty',
		'collectResult',
		'emptyButAllocatedWith20',
		'otherCollection',
		'indexCollection',
		'elementExistsTwice',
		'collectionWithElement',
		'collectionOfFloat',
		'elementNotIn',
		'indexArray',
		'withoutEqualElements',
		'floatCollectionWithSameBeginingEnd',
		'duplicateElement',
		'collectionWithDuplicateElement',
		'collection5Elements',
		'collectionWith4Elements',
		'collectionOfCollectionsOfInts',
		'collectionOfCollectionsOfStrings',
		'withCharacters',
		'collectionNotIncluded',
		'simpleCollection'
	],
	#category : #'Collections-Tests-Sequenceable'
}

{ #category : #helpers }
OrderedCollectionTest >> aValue [

	^ 33
]

{ #category : #requirements }
OrderedCollectionTest >> accessCollection [

	^ indexCollection
]

{ #category : #helpers }
OrderedCollectionTest >> anIndex [

	^ 2
]

{ #category : #'tests - includes' }
OrderedCollectionTest >> anotherElementNotIn [
	^ 42
]

{ #category : #requirements }
OrderedCollectionTest >> anotherElementOrAssociationIn [
	" return an element (or an association for Dictionary ) present  in 'collection' "
	^ self collection  anyOne
]

{ #category : #requirements }
OrderedCollectionTest >> anotherElementOrAssociationNotIn [
	" return an element (or an association for Dictionary )not present  in 'collection' "
	^ elementNotIn 
]

{ #category : #helpers }
OrderedCollectionTest >> anotherValue [

	^ 66
]

{ #category : #setup }
OrderedCollectionTest >> collection [

	^ collectionWith4Elements
]

{ #category : #'tests - creation' }
OrderedCollectionTest >> collectionClass [ 
	
	^ OrderedCollection
]

{ #category : #requirements }
OrderedCollectionTest >> collectionInForIncluding [
	^ self nonEmpty copyWithoutFirst.
]

{ #category : #requirements }
OrderedCollectionTest >> collectionMoreThan1NoDuplicates [
	" return a collection of size 5 without equal elements"
	^ withoutEqualElements 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionMoreThan5Elements [
" return a collection including at least 5 elements"
	
	^collection5Elements 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionNotIncluded [
	^ OrderedCollection new add: elementNotIn ; add: elementNotIn ; yourself.
]

{ #category : #requirements }
OrderedCollectionTest >> collectionOfCollectionsOfInts [
	^collectionOfCollectionsOfInts.
]

{ #category : #requirements }
OrderedCollectionTest >> collectionOfCollectionsOfStrings [
	^collectionOfCollectionsOfStrings.
]

{ #category : #requirements }
OrderedCollectionTest >> collectionOfFloat [
	^ collectionOfFloat 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWith1TimeSubcollection [
" return a collection including 'oldSubCollection'  only one time "
	^ ((OrderedCollection new add: elementNotIn; yourself),self oldSubCollection) add: elementNotIn;yourself   
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWith2TimeSubcollection [
" return a collection including 'oldSubCollection'  two or many time "
	^ (((OrderedCollection new add: elementNotIn; yourself),self oldSubCollection ) add: elementNotIn;yourself),self  oldSubCollection
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWith5Elements [
" return a collection of size 5 including 5 elements"
^ indexCollection 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithCharacters [
	^withCharacters.
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithCopyNonIdentical [
	" return a collection that include elements for which 'copy' return a different object (this is not the case of SmallInteger)"
	^ collectionOfFloat
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithElement [
	"Returns a collection that already includes what is returned by #element."
	^ collectionWithElement
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithElementsToRemove [
	^ nonEmpty copyWithoutFirst.
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithEqualElements [
" return a collecition including atLeast two elements equal"

^collectionWithDuplicateElement 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithNonIdentitySameAtEndAndBegining [
	" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	^ floatCollectionWithSameBeginingEnd 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	^ floatCollectionWithSameBeginingEnd 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithSortableElements [
" return a collection elements that can be sorte ( understanding message ' < '  or ' > ')"
	^ collectionOfFloat 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithoutEqualElements [

" return a collection not including equal elements "
	^ withoutEqualElements 
]

{ #category : #requirements }
OrderedCollectionTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	^ withoutEqualElements 
]

{ #category : #requirements }
OrderedCollectionTest >> element [
	"Returns an object that can be added to the collection returned by #collection."
	^ 5
]

{ #category : #requirements }
OrderedCollectionTest >> elementInCollectionOfFloat [
	^ collectionOfFloat anyOne.
]

{ #category : #requirements }
OrderedCollectionTest >> elementInForElementAccessing [
" return an element inculded in 'accessCollection '"
	^ self accessCollection anyOne
]

{ #category : #requirements }
OrderedCollectionTest >> elementInForIncludesTest [
" return an element included in nonEmpty "
	^ self nonEmpty anyOne
]

{ #category : #requirements }
OrderedCollectionTest >> elementInForIndexAccessing [

^ self accessCollection  anyOne.
]

{ #category : #requirements }
OrderedCollectionTest >> elementNotIn [
	^ elementNotIn 
]

{ #category : #requirements }
OrderedCollectionTest >> elementNotInForElementAccessing [
" return an element not included in 'accessCollection' "
	^ elementNotIn 
]

{ #category : #requirements }
OrderedCollectionTest >> elementNotInForIndexAccessing [

	^ elementNotIn 
]

{ #category : #'tests - includes' }
OrderedCollectionTest >> elementNotInForOccurrences [
	^ 666
]

{ #category : #'tests - copy' }
OrderedCollectionTest >> elementToAdd [
	^ 55
]

{ #category : #requirements }
OrderedCollectionTest >> elementTwiceIn [
	^ super elementTwiceIn
]

{ #category : #requirements }
OrderedCollectionTest >> elementTwiceInForOccurrences [
" return an element included exactly two time in # collectionWithEqualElements"
^ duplicateElement 
]

{ #category : #requirements }
OrderedCollectionTest >> elementsCopyNonIdenticalWithoutEqualElements [
	" return a collection that does niot incllude equal elements ( classic equality )"
	^ collectionOfFloat
]

{ #category : #setup }
OrderedCollectionTest >> empty [
	
	^ empty
]

{ #category : #setup }
OrderedCollectionTest >> emptyButAllocatedWith20 [ 

	^ emptyButAllocatedWith20
]

{ #category : #accessing }
OrderedCollectionTest >> expectedSizeAfterReject [
	^1
]

{ #category : #requirements }
OrderedCollectionTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	^ nonEmpty 	
]

{ #category : #requirements }
OrderedCollectionTest >> firstIndex [
" return an index between 'nonEmpty' bounds that is < to 'second index' "
	^1
]

{ #category : #requirements }
OrderedCollectionTest >> floatCollectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	^ floatCollectionWithSameBeginingEnd 
]

{ #category : #requirements }
OrderedCollectionTest >> indexArray [
	^ indexArray .
]

{ #category : #requirements }
OrderedCollectionTest >> indexInForCollectionWithoutDuplicates [
" return an index between 'collectionWithoutEqualsElements'  bounds"
	^ 2
]

{ #category : #requirements }
OrderedCollectionTest >> indexInNonEmpty [
" return an index between bounds of 'nonEmpty' "

	^ 2
]

{ #category : #requirements }
OrderedCollectionTest >> integerCollection [
" return a collection only including SmallInteger elements"
	^ indexCollection 
]

{ #category : #requirements }
OrderedCollectionTest >> integerCollectionWithoutEqualElements [
" return a collection of integer without equal elements"
	^ withoutEqualElements 
]

{ #category : #requirements }
OrderedCollectionTest >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	^ indexCollection 
]

{ #category : #requirements }
OrderedCollectionTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	^ indexCollection 
]

{ #category : #requirements }
OrderedCollectionTest >> newElement [
"return an element that will be put in the collection in place of another"
	^999
]

{ #category : #setup }
OrderedCollectionTest >> nonEmpty [
	
	^ nonEmpty
]

{ #category : #requirements }
OrderedCollectionTest >> nonEmpty1Element [
" return a collection of size 1 including one element"
	^ OrderedCollection new add:( self nonEmpty anyOne); yourself.
]

{ #category : #requirements }
OrderedCollectionTest >> nonEmptyMoreThan1Element [
" return a collection with more than one element"
	^ withoutEqualElements  .
]

{ #category : #requirements }
OrderedCollectionTest >> nonEmptyWithoutEqualElements [
" return a collection without equal elements "
	^ withoutEqualElements 
]

{ #category : #requirements }
OrderedCollectionTest >> oldSubCollection [
" return a subCollection included in collectionWith1TimeSubcollection .
ex :   subCollection := #( 2 3 4) and collectionWith1TimeSubcollection := #(1 2 3 4 5)"
	^ nonEmpty 
]

{ #category : #requirements }
OrderedCollectionTest >> otherCollection [
	^ otherCollection
]

{ #category : #requirements }
OrderedCollectionTest >> replacementCollection [
" return a collection that will be used to replace 'oldSubcollection' in ' collectionWith1TimeSubcollection'  " 
	^ collectionWith4Elements
]

{ #category : #requirements }
OrderedCollectionTest >> replacementCollectionSameSize [
" return a collection of size (secondIndex - firstIndex + 1)"

| res |
res := OrderedCollection new.
1 to: (self secondIndex - self firstIndex + 1) do:
	[
	:i |
	res add: 99.
	].
	^res.
]

{ #category : #requirements }
OrderedCollectionTest >> result [

^ collectResult 
]

{ #category : #requirements }
OrderedCollectionTest >> secondCollection [
" return a collection that will be the second part of the concatenation"
	^ collectionWith4Elements 
]

{ #category : #requirements }
OrderedCollectionTest >> secondIndex [
" return an index between 'nonEmpty' bounds that is > to 'second index' "
	^2
]

{ #category : #parameters }
OrderedCollectionTest >> selectorToAccessValuePutIn [
	"return the selector of the method that should be invoked to access an element"
	
	^ #first
]

{ #category : #running }
OrderedCollectionTest >> setUp [

	super setUp.
	nonEmpty := OrderedCollection new  add: self valuePutIn; add: self elementTwiceIn; add: self elementTwiceIn; yourself.
	empty := OrderedCollection new. 
	elementNotIn := 99.
	collectionWith4Elements := OrderedCollection new add: 1; add: -2; add: 3; add: 1; yourself.
	indexArray := { 3. 1.}.
	indexCollection := OrderedCollection new add: 1; add: 2;add: 3; add: 4; add:5;  yourself.
	otherCollection := OrderedCollection new add: 1;add: 20; add: 30; yourself.
	withoutEqualElements := OrderedCollection new add: 1;add: 20; add: 30; yourself.
	collectResult := OrderedCollection new add: SmallInteger; add: SmallInteger; add: SmallInteger; yourself.
	collectionOfCollectionsOfStrings:= OrderedCollection new add: (OrderedCollection new add: (OrderedCollection new add: 'foo'; yourself); add: (OrderedCollection new add: 'bar'; yourself); yourself); add: 'zorg'; yourself.
	simpleCollection := OrderedCollection new add: 1; add: 8; add: 3; yourself.
	collectionOfCollectionsOfInts := OrderedCollection new add: 1; add: (OrderedCollection new add: 2; add: 3; yourself); add: (OrderedCollection new add: 4; add: (OrderedCollection new add: 5; add: 6; yourself); yourself); yourself.
	withCharacters := OrderedCollection new add: $a; add: $x; add: $d; add: $c; add: $m; yourself.
	emptyButAllocatedWith20 := OrderedCollection new: 20. 
	collectionWithElement := OrderedCollection new add: self element; yourself.
	collectionOfFloat := OrderedCollection new add: 4.1; add: 7.2; add: 2.5; yourself.
	floatCollectionWithSameBeginingEnd := OrderedCollection new add: 4.1; add: 7.2; add: 4.1 copy ; yourself.
	duplicateElement := 2.
	collectionWithDuplicateElement := OrderedCollection new add: duplicateElement ; add: duplicateElement ; add:4 ; yourself.	

	collection5Elements := OrderedCollection new add: 1; add: 2;  add: 3;  add: 4;  add: 5; yourself.
]

{ #category : #requirements }
OrderedCollectionTest >> simpleCollection [
	^simpleCollection.
]

{ #category : #setup }
OrderedCollectionTest >> sizeCollection [

	^ collectionWith4Elements
]

{ #category : #requirements }
OrderedCollectionTest >> sortedInAscendingOrderCollection [
	"Return a collection sorted in ascending order"

	^ OrderedCollection new
		add: 1;
		add: 2;
		add: 3;
		add: 4;
		add: 5;
		yourself
]

{ #category : #accessing }
OrderedCollectionTest >> speciesClass [
	
	^ OrderedCollection
]

{ #category : #requirements }
OrderedCollectionTest >> subCollectionNotIn [

	^ self collectionNotIncluded .
]

{ #category : #'tests - fixture' }
OrderedCollectionTest >> test0FixtureTConvertAsSetForMultiplinessTest [
	"a collection ofFloat with equal elements:"

	| res |
	self withEqualElements.
	self withEqualElements do: [ :each | self assert: each isFloat ].
	res := true.
	self withEqualElements detect: [ :each | (self withEqualElements occurrencesOf: each) > 1 ] ifNone: [ res := false ].
	self assert: res = true.	"a collection of Float without equal elements:"
	self elementsCopyNonIdenticalWithoutEqualElements.
	self elementsCopyNonIdenticalWithoutEqualElements do: [ :each | self assert: each isFloat ].
	res := true.
	self elementsCopyNonIdenticalWithoutEqualElements
		detect: [ :each | (self elementsCopyNonIdenticalWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAdd [
	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l add: 88.
	self assert: (l =  #(1 2 3 4 88) asOrderedCollection).
	l add: 99.
	self assert: (l =  #(1 2 3 4 88 99) asOrderedCollection). 


]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddAfter [

	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l add: 88 after: 1.
	self assert: (l =  #(1 88 2 3 4) asOrderedCollection).
	l add: 99 after: 2.
	self assert: (l =  #(1 88 2 99 3 4) asOrderedCollection). 


]

{ #category : #'tests - public methods' }
OrderedCollectionTest >> testAddAfterIndex [
	| coll |
	coll := #(1 2 3 4) asOrderedCollection.
	coll add: 77 afterIndex: 0.
	self assert: coll equals: #(77 1 2 3 4) asOrderedCollection.
	coll add: 88 afterIndex: 2.
	self assert: coll equals: #(77 1 88 2 3 4) asOrderedCollection. 
	coll add: 99 afterIndex: coll size.
	self assert: coll equals: #(77 1 88 2 3 4 99) asOrderedCollection. 
	self should: [ coll add: 666 afterIndex: -1 ] raise: Error.
	self should: [ coll add: 666 afterIndex: coll size+1 ] raise: Error.
	
	"Now make room by removing first two and last two elements,
	and see if the illegal bounds test still fails"
	(coll first: 2), (coll last: 2) reversed do: [:e | coll remove: e].
	self should: [ coll add: 666 afterIndex: -1 ] raise: Error.
	self should: [ coll add: 666 afterIndex: coll size+1 ] raise: Error
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddAll [
	"Allows one to add each element of an OrderedCollection at the end of another
	 OrderedCollection"

	| c1 c2 |
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := #(5 6 7 8 9) asOrderedCollection.
	c1 addAll: c2.
	self assert: c1 equals: #(1 2 3 4 5 6 7 8 9) asOrderedCollection
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddAllFirst [
	"Allows one to add each element of an OrderedCollection at the beginning of another OrderedCollection "
	
	| c1 c2 |
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := #(5 6 7 8 9) asOrderedCollection.
	c2 addAllFirst: c1.
	self assert: c2 equals: #(1 2 3 4 5 6 7 8 9) asOrderedCollection
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddAllFirstUnlessAlreadyPresent [
	"Allows one to add each element of an orderedCollection at the beginning of
	another orderedCollection preserving the order but no duplicate element"
	
	| c1 c2 c3 |
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := #(5 6 7 8 9) asOrderedCollection.
	c3 := #(0 1) asOrderedCollection.
	c2 addAllFirstUnlessAlreadyPresent: c1.
	self assert: c2 equals: #(1 2 3 4 5 6 7 8 9) asOrderedCollection.
	c1 addAllFirstUnlessAlreadyPresent: c3.
	self deny: c1 = #(0 1 1 2 3 4) asOrderedCollection.
	self assert: c1 equals: #(0 1 2 3 4) asOrderedCollection.
	
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddAllLast [
	"Allows one to add each element of an OrderedCollection at the beginning of another OrderedCollection"

	| c1 c2 |
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := #(5 6 7 8 9) asOrderedCollection.
	c1 addAllLast: c2.
	self assert: c1 equals: #(1 2 3 4 5 6 7 8 9) asOrderedCollection
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddBefore [

	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l add: 88 before: 1.
	self assert: (l =  #(88 1 2 3 4) asOrderedCollection).
	l add: 99 before: 2.
	self assert: (l =  #(88 1 99 2 3 4) asOrderedCollection). 


]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddBeforeAndRemove [

	| l initialCollection |
	l := #(1 2 3 4) asOrderedCollection.
	initialCollection := l shallowCopy.
	l add: 88 before: 1.
	self assert: (l =  #(88 1 2 3 4) asOrderedCollection).
	l add: 99 before: 2.
	self assert: (l =  #(88 1 99 2 3 4) asOrderedCollection). 
	l remove: 99.
	l remove: 88.
	self assert: l = initialCollection.


]

{ #category : #'tests - public methods' }
OrderedCollectionTest >> testAddBeforeIndex [

	| coll |
	coll := #(1 2 3 4) asOrderedCollection.
	coll add: 77 beforeIndex: 1.
	self assert: coll equals: #(77 1 2 3 4) asOrderedCollection.
	coll add: 88 beforeIndex: 3.
	self assert: coll equals: #(77 1 88 2 3 4) asOrderedCollection. 
	coll add: 99 beforeIndex: coll size+1.
	self assert: coll equals: #(77 1 88 2 3 4 99) asOrderedCollection. 
	self should:[ coll add: 666 beforeIndex: 0 ] raise: Error.
	self should:[ coll add: 666 beforeIndex: coll size+2 ] raise: Error.
	
	"Now make room by removing first two and last two elements,
	and see if the illegal bounds test still fails"
	(coll first: 2), (coll last: 2) reversed do: [:e | coll remove: e].
	self should:[ coll add: 666 beforeIndex: 0 ] raise: Error.
	self should:[ coll add: 666 beforeIndex: coll size+2 ] raise: Error
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddDuplicateItem1 [
	| collection |
	collection := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
	collection add: 'John' before: 'John'.
	self
		assert: ((collection asBag occurrencesOf: 'John')
					= 2
				and: [(collection at: (collection indexOf: 'John')
							+ 1)
						= (collection
								at: (collection indexOf: 'John'))])
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddFirst [
	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l addFirst: 88.
	self assert: (l =  #(88 1 2 3 4) asOrderedCollection).
	l addFirst: 99.
	self assert: (l =  #(99 88 1 2 3 4) asOrderedCollection). 


]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddItem1 [

   | collection size |
   collection := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
   size := collection size.
   collection add: 'James' before: 'Jim'.
   collection add: 'Margaret' before: 'Andrew'.
   self assert: size + 2 = collection size.

]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddItem2 [
	| collection |
	collection := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
	collection add: 'James' before: 'Jim'.
	collection add: 'Margaret' before: 'Andrew'.
	self assert: (collection indexOf: 'James')
			+ 1
			= (collection indexOf: 'Jim').
	self assert: (collection indexOf: 'Margaret')
			+ 1
			= (collection indexOf: 'Andrew')
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAddLast [
	| l |
	l := #(1 2 3 4) asOrderedCollection.
	l addLast: 88.
	self assert: (l =  #(1 2 3 4 88) asOrderedCollection).
	l addLast: 99.
	self assert: (l =  #(1 2 3 4 88 99) asOrderedCollection). 


]

{ #category : #'tests - converting' }
OrderedCollectionTest >> testAsOrderedCollectionKeepsIdentity [
	
	|x y|
	x := (1 to: 5) asOrderedCollection.
	y := x asOrderedCollection.
	self assert: y identityHash = x identityHash  
]

{ #category : #'tests - converting' }
OrderedCollectionTest >> testAsOrderedCollectionReturnsOrderedCollection [
	
	self assert: #(5 4 3 ) asSortedCollection asOrderedCollection species == OrderedCollection 
]

{ #category : #'tests - accessing' }
OrderedCollectionTest >> testAt [
	| collection |
	collection := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
	self assert: (collection at:1) = 'Jim'.
	self assert: (collection at:2) = 'Mary'
]

{ #category : #'tests - adding' }
OrderedCollectionTest >> testAtIfAbsentPut [
	"Allows one to add an element at an index if no element exist at this index"

	| c |
	c := #(1 2 3 4) asOrderedCollection.
	c at: 2 ifAbsentPut: [ 5 ].
	self assert: c equals: #(1 2 3 4) asOrderedCollection.
	c at: 5 ifAbsentPut: [ 5 ].
	self assert: c equals: #(1 2 3 4 5) asOrderedCollection.
	c at: 7 ifAbsentPut: [ 7 ].
	self assert: c equals: #(1 2 3 4 5 nil 7) asOrderedCollection
]

{ #category : #'tests - accessing' }
OrderedCollectionTest >> testAtPut [
	"Allows one to replace an element but not at an off range index"

	| c |
	c := #(1 2 3 4) asOrderedCollection.
	c at: 2 put: 5.
	self assert: c equals: #(1 5 3 4) asOrderedCollection.
	self
		should: [ c at: 5 put: 8 ]
		raise: Error.
	self deny: c = #(1 5 3 4 8) asOrderedCollection
]

{ #category : #'tests - begin' }
OrderedCollectionTest >> testBeginsWithAnyOf [
	"We can't test SequenceableCollection directly. However, we can test a sampling of its descendants."
	
	| la oc |
	la := #(1 2 3 4 5 6).
	oc := OrderedCollection new.
	oc add: 1; add: 2; add: 3.
	
	self assert: (la beginsWithAnyOf: #((17) (1) (42))).
	self assert: (la beginsWithAnyOf: #((17) (1 2) (42))).
	self assert: (la beginsWithAnyOf: #((17) (1 2 3) (42))).
	self deny: (la beginsWithAnyOf: #()).
	self deny: (la beginsWithAnyOf: #(())).
	self deny: (la beginsWithAnyOf: #((42))).
]

{ #category : #'tests - accessing' }
OrderedCollectionTest >> testCapacityFromAsOrderedCollection [
	"Allows one to check the current capacity of an Ordered collection"
	
	| c1 c2 c3 |
	c1 := #(1 2 ) asOrderedCollection.
	self assert: c1 capacity equals: 2.
	c2 := OrderedCollection new: 10.
	c2 add: 3.
	self assert: c2 capacity equals: 10.	
	c3 := OrderedCollection new.
	self deny: c3 capacity equals: 0
]

{ #category : #'tests - enumerating' }
OrderedCollectionTest >> testCollect [
	"Allows one to collect some element of a collection into another collection"
	| c1 c2 res |
	c1 := #(-1 2 -3 4) asOrderedCollection.
	c2 := #(1 2 3 4) asOrderedCollection.
	res := c1 collect: [:each | each abs].
	self assert: c2 equals: res
]

{ #category : #'tests - enumerating' }
OrderedCollectionTest >> testCollectFromTo [
	"Allows one to collect some element of a collection into another collection between a first index and an end index for the collect"
	
	| c1 res |
	c1 := #(-1 2 -3 4 -5 6 -7 8) asOrderedCollection.
	res := c1 collect: [:each | each abs] from: 1 to: 3.
	self assert: res equals: #(1 2 3) asOrderedCollection.
	self should: [c1 collect: [:each | each abs] from: 10 to: 13] raise: Error.
	self should: [c1 collect: [:each | each abs] from: 5 to: 2] raise: Error.
]

{ #category : #'tests - copying' }
OrderedCollectionTest >> testCopyEmptyOld [
	"Allows one to create a copy of the receiver that contains no elements"

	| c1 c2 |
	c1 := #(1 2 3 4 ) asOrderedCollection.
	c2 := c1 copyEmpty.
	self assert: c2 size equals: 0
]

{ #category : #'tests - copying' }
OrderedCollectionTest >> testCopyFromTo [
	"Allows one to create a copy of the receiver that contains elements from position start to end"
	
	| c1 c2 c3 | 
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := (c1 copyFrom: 1 to: 2).
	self assert: c2 equals: #(1 2) asOrderedCollection.
	self should: [c1 copyFrom: 10 to: 20] raise: Error.
	
	c3 := c1 copyFrom: 4 to: 2.
	self assert: c3 isEmpty.
	
	self should: [c1 copyFrom: 4 to: 5 ] raise: Error.
	
	
	

]

{ #category : #'tests - copy' }
OrderedCollectionTest >> testCopyNonEmptyWithoutAllNotIncluded [
	
]

{ #category : #'tests - copying' }
OrderedCollectionTest >> testCopyReplaceFromToWith [
	"Allows one to create a copy from the receiver which elements between start and end of the receiver being replace by element of the collection after with:"

	| c1 c2 c3 c4 |
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := #(5 6 7 8 9) asOrderedCollection.
	c3 := (c2 copyReplaceFrom: 1 to: 2 with: c1).
	self assert: c3 equals: #(1 2 3 4 7 8 9) asOrderedCollection.
	self should: [c2 copyReplaceFrom: 3 to: 1 with: c1] raise: Error.
	
	c4 := (c2 copyReplaceFrom: 10 to: 25 with: c1).
	self assert: c4 equals: #(5 6 7 8 9 1 2 3 4) asOrderedCollection
]

{ #category : #'tests - copying' }
OrderedCollectionTest >> testCopyWith [
	"Allows one to create a copy of the receiver that contains the new element at the end"
	
	| c1 | 
	c1 := #(1 2 3 4) asOrderedCollection.
	c1 := c1 copyWith: 6.
	self assert: c1 equals: #(1 2 3 4 6) asOrderedCollection
	
	

	
	
	

]

{ #category : #'tests - begin' }
OrderedCollectionTest >> testEndsWithAnyOf [
	"We can't test SequenceableCollection directly. However, we can test a sampling of its descendants."
	
	| la oc |
	la := #(6 5 4 3 2 1).
	oc := OrderedCollection new.
	oc add: 3; add: 2; add: 1.
	
	self assert: (la endsWithAnyOf: #((17) (1) (42))).
	self assert: (la endsWithAnyOf: #((17) (2 1) (42))).
	self assert: (la endsWithAnyOf: #((17) (3 2 1) (42))).
	self deny: (la endsWithAnyOf: #()).
	self deny: (la endsWithAnyOf: #(())).
	self deny: (la endsWithAnyOf: #((42))).
]

{ #category : #'tests - includes' }
OrderedCollectionTest >> testIdentityIncludes [
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "

	" can not be used by collections that can't include elements for wich copy doesn't return another instance "

	| collection element |
	self collectionWithCopyNonIdentical.
	collection := self collectionWithCopyNonIdentical.
	element := collection anyOne copy.	"self assert: (collection includes: element)."
	self deny: (collection identityIncludes: element)
]

{ #category : #'tests - includes' }
OrderedCollectionTest >> testIncludesElementIsNotThere [
	
	self deny: (self nonEmpty includes: self elementNotInForOccurrences).
	self assert: (self nonEmpty includes: self nonEmpty anyOne).
	self deny: (self empty includes: self elementNotInForOccurrences)
]

{ #category : #'tests - enumerating' }
OrderedCollectionTest >> testIndexOfWithDuplicates [

   | collection indices bagOfIndices |
   collection := #('Jim' 'Mary' 'John' 'Andrew' 'Mary' 'John' 'Jim' 'Micheal') asOrderedCollection.
   indices := collection collect: [:item | collection indexOf: item].
   self assert: indices asSet size = collection asSet size.
   bagOfIndices := indices asBag.
   self assert: (indices asSet 
                    allSatisfy: [:index | (bagOfIndices occurrencesOf: index)
	                                       = (collection occurrencesOf: (collection at: index))]).

  "  indexOf:  returns the index of the first occurrence of an item.
     For an item with n occurrences, the index of its first occurrence
     is found  n  times. "
]

{ #category : #'tests - removing' }
OrderedCollectionTest >> testRemoveAll [
	"Allows one to remove all elements of a collection" 
	
	| c1 c2 s2 |
	c1 := #(2 3 4 6) asOrderedCollection.
	c1 addAll: (1 to: 200).
	c2 := c1 copy.
	s2 := c2 size.
	
	c1 removeAll.
	
	self assert: c1 size = 0.
	self assert: c2 size = s2 description: 'the copy has not been modified'
	
]

{ #category : #'tests - removing' }
OrderedCollectionTest >> testRemoveAllSuchThat [
	| collection |
	collection := (1 to: 10) asOrderedCollection.
	collection
		removeAllSuchThat: [:e | e even].
	self assert: collection = (1 to: 10 by: 2) asOrderedCollection
]

{ #category : #'tests - removing' }
OrderedCollectionTest >> testRemoveAt [
	"Allows one to remove an element from a collection at an index"
	
	| c1 |
	c1 := #(2 3 4 6) asOrderedCollection.
	c1 removeAt: 2.
	self assert: c1 equals: #(2 4 6) asOrderedCollection.
	self should: [ c1 removeAt: 10 ] raise: Error.
	self should: [ c1 removeAt: -1 ] raise: Error
	
]

{ #category : #'tests - removing' }
OrderedCollectionTest >> testRemoveFirst [
	"Allows one to remove n element of a collection at the first"
	
	| c1 |
	c1 := #(2 3 4 6) asOrderedCollection.
	c1 removeFirst: 1.
	self assert: c1 equals: #(3 4 6) asOrderedCollection.
	c1 removeFirst: 2.
	self assert: c1 equals: #(6) asOrderedCollection.
	self should: [c1 removeFirst: 10] raise: Error
	
]

{ #category : #'tests - removing' }
OrderedCollectionTest >> testRemoveIfAbsent [
	"Allows one to remove an element from a collection and to copy it in another collection."
	"If the element isn't in the first collection, the second collection copy the element after ifAbsent"
	
	| c1 c2 |
	c1 := #(1 2 3 4) asOrderedCollection.
	c2 := OrderedCollection new.
	
	c2 add: (c1 remove: 2 ifAbsent: [6]).
	self assert: c1 equals: #(1 3 4) asOrderedCollection.
	self assert: c2 equals: #(2) asOrderedCollection.
	
	c2 add: (c1 remove: 18 ifAbsent: [6]).
	self assert: c1 equals: #(1 3 4) asOrderedCollection.
	self assert: c2 equals: #(2 6) asOrderedCollection
]

{ #category : #'tests - removing' }
OrderedCollectionTest >> testRemoveLast [
	"Allows one to remove n element of a collection at the end"
	
	| c1 |
	c1 := #(2 3 4 6) asOrderedCollection.
	c1 removeLast: 1.
	self assert: c1 equals: #(2 3 4) asOrderedCollection.
	c1 removeLast: 2.
	self assert: c1 equals: #(2) asOrderedCollection.
	self should: [ c1 removeLast: 10 ] raise: Error
]

{ #category : #'tests - copying' }
OrderedCollectionTest >> testReversed [
	| collection1 collection2 |
	collection1 := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
	collection2 := collection1 reversed.
	self assert: collection2 first = 'Andrew'.
	self assert: collection2 last = 'Jim'
]

{ #category : #tests }
OrderedCollectionTest >> testSort [	
	| ord |
	ord := OrderedCollection new addAll: #(2 1 3 6 7 10 6); yourself.
	self assert: ord sort asArray equals: #(1 2 3 6 6 7 10). 
	self assert: ord sort equals: (OrderedCollection new addAll:#(1 2 3 6 6 7 10); yourself). 
	self assert: (ord sort: [:a :b | a > b]) asArray equals: #(10 7 6 6 3 2 1).
	ord := OrderedCollection new.
	self assert: ord sort asArray equals: #(). 

]

{ #category : #'tests - enumerating' }
OrderedCollectionTest >> testWithCollect [
	"Allows one to collect some element of two collections into another collection with element corresponding to the condition in the blocks"
	
	| c1 c2 res |
	c1 := #(-1 2 -3 4 -5 6 -7 8) asOrderedCollection.
	c2 := #(-9 10 -11 12 -13 14 -15 16) asOrderedCollection.
	res := c1 with: c2 collect: [:each1 :each2 | each1 min: each2 ].
	self assert: res equals: #(-9 2 -11 4 -13 6 -15 8) asOrderedCollection
]

{ #category : #requirements }
OrderedCollectionTest >> unsortedCollection [
	^ OrderedCollection new
		add: 3;
		add: 1;
		add: 20;
		add: 4;
		add: 2;
		yourself
]

{ #category : #requirements }
OrderedCollectionTest >> withEqualElements [
	" return a collection of float including equal elements (classic equality)"
	^ collectionOfFloat , collectionOfFloat
]
