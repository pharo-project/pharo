"
SUnit tests for sorted collections
"
Class {
	#name : #SortedCollectionTest,
	#superclass : #CollectionRootTest,
	#traits : 'TIterateSequencedReadableTest + TPrintOnSequencedTest + TAsStringCommaAndDelimiterSequenceableTest + TIncludesWithIdentityCheckTest + (TSequencedElementAccessTest - {#testAtAll}) + TSubCollectionAccess + TIndexAccessForMultipliness + TRemoveTest + TConvertTest + TConvertAsSetForMultiplinessIdentityTest + TAddTest + TBeginsEndsWith + (TCopySequenceableSameContents - {#testShuffled}) + TSetArithmetic + TRemoveByIndexTest + TCopyPartOfSequenceable + TCopyPartOfSequenceableForMultipliness + (TCopySequenceableWithOrWithoutSpecificElements - {#testForceToPaddingStartWith. #testForceToPaddingWith. #testCopyWithFirst. #testCopyWithoutIndex}) + TCopySequenceableWithReplacementForSorted + TCopyTest + TConvertAsSortedTest + TSequencedStructuralEqualityTest + TCreationWithTest + TOccurrencesForMultiplinessTest',
	#classTraits : 'TIterateSequencedReadableTest classTrait + TPrintOnSequencedTest classTrait + TAsStringCommaAndDelimiterSequenceableTest classTrait + TIncludesWithIdentityCheckTest classTrait + TSequencedElementAccessTest classTrait + TSubCollectionAccess classTrait + TIndexAccessForMultipliness classTrait + TRemoveTest classTrait + TConvertTest classTrait + TConvertAsSetForMultiplinessIdentityTest classTrait + TAddTest classTrait + TBeginsEndsWith classTrait + TCopySequenceableSameContents classTrait + TSetArithmetic classTrait + TRemoveByIndexTest classTrait + TCopyPartOfSequenceable classTrait + TCopyPartOfSequenceableForMultipliness classTrait + TCopySequenceableWithOrWithoutSpecificElements classTrait + TCopySequenceableWithReplacementForSorted classTrait + TCopyTest classTrait + TConvertAsSortedTest classTrait + TSequencedStructuralEqualityTest classTrait + TCreationWithTest classTrait + TOccurrencesForMultiplinessTest classTrait',
	#instVars : [
		'nonEmpty',
		'elementExistsTwice',
		'empty',
		'collectResult',
		'nonEmpty1Element',
		'collectionOfFloat',
		'accessCollection',
		'elementNoteIn',
		'oldSubcollection',
		'floatCollectionSameEndAndBegining',
		'withoutEqualElements',
		'collectionOfFloatWithDuplicate',
		'collectionIncluded',
		'collectionNotIncluded',
		'collectionWithoutNil',
		'duplicateFloat',
		'nonEmpty5Elements',
		'collectionWith4Elements'
	],
	#category : #'Collections-Tests-Unordered'
}

{ #category : #requirements }
SortedCollectionTest >> accessCollection [
"Return a collection of size 5"
	^accessCollection 
]

{ #category : #requirements }
SortedCollectionTest >> anotherElementNotIn [
" return an element different of 'elementNotIn'  not included in 'nonEmpty' "
	^666
]

{ #category : #requirements }
SortedCollectionTest >> anotherElementOrAssociationIn [
	" return an element (or an association for Dictionary ) present  in 'collection' "
	^ self collection anyOne
]

{ #category : #requirements }
SortedCollectionTest >> anotherElementOrAssociationNotIn [
	" return an element (or an association for Dictionary )not present  in 'collection' "
	^ elementNoteIn 
]

{ #category : #requirements }
SortedCollectionTest >> collection [

	^ collectionWith4Elements
]

{ #category : #requirements }
SortedCollectionTest >> collectionClass [
" return the class to be used to create instances of the class tested"
	^ SortedCollection
]

{ #category : #requirements }
SortedCollectionTest >> collectionMoreThan1NoDuplicates [
	" return a collection of size 5 without equal elements"
	^ withoutEqualElements 
]

{ #category : #requirements }
SortedCollectionTest >> collectionMoreThan5Elements [
" return a collection including at least 5 elements"
	
	^ nonEmpty5Elements 
]

{ #category : #requirements }
SortedCollectionTest >> collectionNotIncluded [
" return a collection for wich each element is not included in 'nonEmpty' "
	^ collectionNotIncluded 
]

{ #category : #requirements }
SortedCollectionTest >> collectionOfFloat [
" return a collection only includiing Float elements "
	^ collectionOfFloat 
]

{ #category : #requirements }
SortedCollectionTest >> collectionOfSize5 [
" return a collection of size 5"
^ accessCollection 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWith1TimeSubcollection [
" return a collection including 'oldSubCollection'  only one time "
	^ (SortedCollection new add: elementNoteIn ; yourself) , self oldSubCollection 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWith2TimeSubcollection [
" return a collection including 'oldSubCollection'  two or many time "
	^ (SortedCollection  new add: elementNoteIn ; yourself) , self oldSubCollection , self oldSubCollection  
]

{ #category : #requirements }
SortedCollectionTest >> collectionWith5Elements [
" return a collection of size 5 including 5 elements"
^ accessCollection 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithCopyNonIdentical [
	" return a collection that include elements for which 'copy' return a different object (this is not the case of SmallInteger)"
	^ collectionOfFloat
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithElement [
	"Returns a collection that already includes what is returned by #element."
	^ SortedCollection new add: self element ; add: 5 ; add: 2; yourself.
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithElementsToRemove [
" return a collection of elements included in 'nonEmpty'  "
	^ collectionIncluded 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithEqualElements [
" return a collecition including atLeast two elements equal"

^ collectionOfFloatWithDuplicate 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithNonIdentitySameAtEndAndBegining [
	" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	^ floatCollectionSameEndAndBegining 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	^ floatCollectionSameEndAndBegining 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithSortableElements [
" return a collection elements that can be sorte ( understanding message ' < '  or ' > ')"
	^ collectionOfFloat 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithoutEqualElements [

" return a collection not including equal elements "
	^withoutEqualElements 
]

{ #category : #requirements }
SortedCollectionTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	^ collectionWithoutNil  
]

{ #category : #requirements }
SortedCollectionTest >> element [
	"Returns an object that can be added to the collection returned by #collection."
	^ 88
]

{ #category : #requirements }
SortedCollectionTest >> elementInCollectionOfFloat [
" return an element included in 'collectionOfFloat' "
	^ collectionOfFloat anyOne
]

{ #category : #requirements }
SortedCollectionTest >> elementInForElementAccessing [
" return an element inculded in 'accessCollection '"
	^ self accessCollection anyOne
]

{ #category : #requirements }
SortedCollectionTest >> elementInForIncludesTest [
" return an element included in nonEmpty "
	^ nonEmpty anyOne 
]

{ #category : #requirements }
SortedCollectionTest >> elementInForIndexAccessing [
" return an element included in 'accessCollection' "
	^ self collectionMoreThan1NoDuplicates  anyOne
]

{ #category : #requirements }
SortedCollectionTest >> elementNotIn [
"return an element not included in 'nonEmpty' "

	^ elementNoteIn 
]

{ #category : #requirements }
SortedCollectionTest >> elementNotInForElementAccessing [
" return an element not included in 'accessCollection' "
	^ elementNoteIn 
]

{ #category : #requirements }
SortedCollectionTest >> elementNotInForIndexAccessing [
" return an element not included in 'accessCollection' "
	^ elementNoteIn  	
]

{ #category : #requirements }
SortedCollectionTest >> elementNotInForOccurrences [
	^ 666
]

{ #category : #requirements }
SortedCollectionTest >> elementToAdd [
" return an element of type 'nonEmpy' elements'type'"
	^ 5
]

{ #category : #requirements }
SortedCollectionTest >> elementTwiceInForOccurrences [
" return an element included exactly two time in # collectionWithEqualElements"
^ duplicateFloat 
]

{ #category : #requirements }
SortedCollectionTest >> elementsCopyNonIdenticalWithoutEqualElements [
	" return a collection that does niot incllude equal elements ( classic equality )"
	^ collectionOfFloat
]

{ #category : #requirements }
SortedCollectionTest >> empty [
	
	^ empty
]

{ #category : #requirements }
SortedCollectionTest >> expectedSizeAfterReject [
	^1
]

{ #category : #requirements }
SortedCollectionTest >> floatCollectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	^ floatCollectionSameEndAndBegining 
]

{ #category : #requirements }
SortedCollectionTest >> indexInForCollectionWithoutDuplicates [
" return an index between 'collectionWithoutEqualsElements'  bounds"
	^ 2
]

{ #category : #requirements }
SortedCollectionTest >> indexInNonEmpty [
" return an index between bounds of 'nonEmpty' "

	^ 2
]

{ #category : #requirements }
SortedCollectionTest >> integerCollection [
" return a collection only including SmallInteger elements"
	^ accessCollection 
]

{ #category : #requirements }
SortedCollectionTest >> integerCollectionWithoutEqualElements [
" return a collection of integer without equal elements"
	^ withoutEqualElements 
]

{ #category : #requirements }
SortedCollectionTest >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	^ accessCollection 
]

{ #category : #requirements }
SortedCollectionTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	^ accessCollection 
]

{ #category : #requirements }
SortedCollectionTest >> nonEmpty [
	
	^ nonEmpty
]

{ #category : #requirements }
SortedCollectionTest >> nonEmpty1Element [
" return a collection of size 1 including one element"
	^ nonEmpty1Element 
]

{ #category : #requirements }
SortedCollectionTest >> nonEmptyMoreThan1Element [
" return a collection of integer with more than one element" 
	^withoutEqualElements 
	.
]

{ #category : #requirements }
SortedCollectionTest >> nonEmptyWithoutEqualElements [
" return a collection without equal elements "
	^ withoutEqualElements 
]

{ #category : #requirements }
SortedCollectionTest >> oldSubCollection [
" return a subCollection included in collectionWith1TimeSubcollection .
ex :   subCollection := #( 2 3 4) and collectionWith1TimeSubcollection := #(1 2 3 4 5)"
	^ oldSubcollection 
]

{ #category : #requirements }
SortedCollectionTest >> otherCollection [
	"Returns a collection that does not include what is returned by #element."
	 ^ SortedCollection new add:  7 ; add: 5 ; add: 2; yourself.
]

{ #category : #requirements }
SortedCollectionTest >> replacementCollection [
" return a collection that will be used to replace 'oldSubcollection' in ' collectionWith1TimeSubcollection'  " 
	^ collectionWith4Elements  
]

{ #category : #requirements }
SortedCollectionTest >> result [

	 ^ collectResult
]

{ #category : #running }
SortedCollectionTest >> setUp [
	super setUp.
	nonEmpty := SortedCollection new.
	elementExistsTwice := 12332312321.
	nonEmpty add: 2.
	nonEmpty add: elementExistsTwice.
	nonEmpty add: elementExistsTwice.
	collectionIncluded := SortedCollection new add: 2; add: elementExistsTwice ;yourself.
	collectionNotIncluded := SortedCollection new add: 312; add: 313 ;yourself.
	empty := SortedCollection  new. 
	collectionWith4Elements := SortedCollection new.
	collectionWith4Elements add: 1.
	collectionWith4Elements add: -2.
	collectionWith4Elements add: 3.
	collectionWith4Elements add: 1.
	collectionWithoutNil := SortedCollection new add: 1;add: 2 ;add:4 ;add:5;yourself.
	collectResult := OrderedCollection new. "SortedCollection sortBlock: [:a :b | a name < b name]."
	collectResult add: SmallInteger.
	collectResult add: SmallInteger.
	collectResult add: SmallInteger.
	collectResult add: SmallInteger.
	nonEmpty1Element := SortedCollection new add:5; yourself.
	collectionOfFloat := SortedCollection new add:1.2 ; add: 5.6 ; add:4.4 ; add: 1.9 ; yourself.
	duplicateFloat := 1.2.
	collectionOfFloatWithDuplicate := SortedCollection new add: duplicateFloat  ; add: 5.6 ; add:4.4 ; add: duplicateFloat  ; yourself.
	accessCollection := SortedCollection new add:1 ; add: 5 ; add:4 ; add: 2 ; add:7 ; yourself.
	elementNoteIn := 999.
	oldSubcollection := SortedCollection new add: 2 ; add: 2 ; add: 2 ; yourself.
	floatCollectionSameEndAndBegining := SortedCollection new add: 1.5 ; add: 1.5 copy ; yourself.
	withoutEqualElements := SortedCollection new add: 1 ; add: 8 copy ; add: 4;yourself.
	nonEmpty5Elements := SortedCollection new add: 1 ; add: 8 copy ; add: 4; add: 4; add: 4;yourself.
]

{ #category : #requirements }
SortedCollectionTest >> sizeCollection [
	^ collectionWith4Elements 
]

{ #category : #requirements }
SortedCollectionTest >> speciesClass [
	
	^ SortedCollection
]

{ #category : #requirements }
SortedCollectionTest >> subCollectionNotIn [
" return a collection for which at least one element is not included in 'accessCollection' "
	^ SortedCollection new add: elementNoteIn ; add: elementNoteIn ; yourself.
]

{ #category : #'tests - fixture' }
SortedCollectionTest >> test0FixtureTConvertAsSetForMultiplinessTest [
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

{ #category : #'tests - basic' }
SortedCollectionTest >> testAdd [
		
	| collection |
	collection := #(10 9 8 7 5 4 4 2) asSortedCollection.
	self assert: collection first equals: 2.
	self assert: collection last equals: 10.
	self assert: collection size equals: 8.
	collection add:1.
	self assert: collection size equals: 9.
	collection add: 6.
	self assert: (collection at: 5) equals: 5.
	self assert: collection size equals: 10.
	collection add: 3.
	
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testAddAll [

	| sorted2 sorted|
	sorted2 := SortedCollection new.
	sorted2 
		add: 'brochet'; 
		add:'truitelle'.
	sorted := SortedCollection new.
	sorted addAll: sorted2.
	self assert: (sorted hasEqualElements: sorted2).
	 
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testAddAll2 [
	
	| sorted2 sorted|
	sorted2 := SortedCollection new.
	sorted2 add: 'brochet'; add:'truitelle'.
	sorted := SortedCollection new.
	sorted add: 'perche'.
	sorted addAll: sorted2.
	self assert: sorted size equals: (sorted2 size + 1).
	sorted2 do: [ :each | self assert: (sorted includes: each)]
	 
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testCollect [
	
	|result aSortedCollection|
	aSortedCollection := SortedCollection new.
	result := OrderedCollection new.
	result 
		add: true; 
		add: true; 
		add: true;
		add: false; 
		add: false.
	
	aSortedCollection := (1 to: 5) asSortedCollection.
	self assert: result equals: (aSortedCollection collect: [:each | each < 4])
	
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testCopy [
		
	|aSortedCollection copySorted|
	aSortedCollection := SortedCollection new.
	aSortedCollection sortBlock: [:a :b | a < b].
	aSortedCollection 
		add: 'truite'; 
		add: 'brochet'.
	self assert: aSortedCollection first equals: 'brochet'.
	
	copySorted := aSortedCollection copy.
	
	self assert: (copySorted  hasEqualElements: aSortedCollection).
	self assert: copySorted species equals: aSortedCollection species.
	self assert: copySorted sortBlock equals: aSortedCollection sortBlock.
	self assert: copySorted first = 'brochet'
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testCreation [
		
	| collection |
	collection := #(10 9 3 6 1 8 7 5 4 2) asSortedCollection.
	self assert: collection equals: (1 to: 10) asSortedCollection.
	
]

{ #category : #'tests - iterate' }
SortedCollectionTest >> testDo2 [

	| res |
	res := self speciesClass sortBlock: [:a :b | a name < b name]..  
	self collection do: [:each | res add: each class].
	self assert: res asArray = self result asArray. 

]

{ #category : #'tests - basic' }
SortedCollectionTest >> testEquals [
		
	|aSortedCollection|
	aSortedCollection := SortedCollection new.
	aSortedCollection add: 'truite'; add: 'brochet'.
	self assert: aSortedCollection copy equals: aSortedCollection.
]

{ #category : #'tests - enumerating' }
SortedCollectionTest >> testFlatCollect [
	self assert: (#(1 2) asSortedCollection flatCollect: [ :x | { x } ]) = #(1 2) asOrderedCollection
]

{ #category : #'tests - includes' }
SortedCollectionTest >> testIdentityIncludes [
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "

	" can not be used by collections that can't include elements for wich copy doesn't return another instance "

	| collection element |
	self collectionWithCopyNonIdentical.
	collection := self collectionWithCopyNonIdentical.
	element := collection anyOne copy.	"self assert: (collection includes: element)."
	self deny: (collection identityIncludes: element)
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testIdentityIndexOf [
	
	| collection element |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection identityIndexOf: element) = (collection indexOf: element)
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testIdentityIndexOfIAbsent [
	| collection element |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection 
			identityIndexOf: element
			ifAbsent: [ 0 ]) = 1.
	self assert: (collection 
			identityIndexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 55 ]) = 55
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testIndexOf [
	
	| tmp index collection |
	collection := self collectionMoreThan1NoDuplicates.
	tmp := collection size.
	collection reverseDo: 
		[ :each | 
		each = self elementInForIndexAccessing ifTrue: [ index := tmp ].
		tmp := tmp - 1 ].
	self assert: (collection indexOf: self elementInForIndexAccessing) = index
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testIndexOfIfAbsent [
	
	| collection |
	collection := self collectionMoreThan1NoDuplicates.
	self assert: (collection 
			indexOf: collection first
			ifAbsent: [ 33 ]) = 1.
	self assert: (collection 
			indexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 33 ]) = 33
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testIndexOfStartingAt [
	 
	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection 
			indexOf: element
			startingAt: 2
			ifAbsent: [ 99 ]) = 99.
	self assert: (collection 
			indexOf: element
			startingAt: 1
			ifAbsent: [ 99 ]) = 1.
	self assert: (collection 
			indexOf: self elementNotInForIndexAccessing
			startingAt: 1
			ifAbsent: [ 99 ]) = 99
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testIndexOfSubCollectionStartingAt [
	 
	| subcollection index collection |
	collection := self collectionMoreThan1NoDuplicates.
	subcollection := self collectionMoreThan1NoDuplicates.
	index := collection 
		indexOfSubCollection: subcollection
		startingAt: 1.
	self assert: index equals: 1.
	index := collection 
		indexOfSubCollection: subcollection
		startingAt: 2.
	self assert: index equals: 0
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testIndexOfSubCollectionStartingAtIfAbsent [
	 
	| index absent subcollection collection |
	collection := self collectionMoreThan1NoDuplicates.
	subcollection := self collectionMoreThan1NoDuplicates.
	absent := false.
	index := collection 
		indexOfSubCollection: subcollection
		startingAt: 1
		ifAbsent: [ absent := true ].
	self assert: absent equals: false.
	absent := false.
	index := collection 
		indexOfSubCollection: subcollection
		startingAt: 2
		ifAbsent: [ absent := true ].
	self assert: absent equals: true
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testLastIndexOf [
	
	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection lastIndexOf: element) = 1.
	self assert: (collection lastIndexOf: self elementNotInForIndexAccessing) = 0
]

{ #category : #'tests - index access' }
SortedCollectionTest >> testLastIndexOfStartingAt [

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection last.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 99 ]) = collection size.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 99 ]) = 99.
	self assert: (collection 
			lastIndexOf: self elementNotInForIndexAccessing
			startingAt: collection size
			ifAbsent: [ 99 ]) = 99
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testMedian [
	
	|aSortedCollection|
	aSortedCollection := (1 to: 10) asSortedCollection.
	self assert: aSortedCollection median equals: 5.
	
	aSortedCollection := SortedCollection new.
	aSortedCollection 
		add: 'truite'; 
		add: 'porcinet'; 
		add: 'carpe'.

	self assert: aSortedCollection median equals: 'porcinet'.
	
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testRemoveAll [
	"Allows one to remove all elements of a collection" 
	
	| c1 c2 s2 |
	c1 := #(10 9 8 7 5 4 4 2) asSortedCollection: [:a :b | a >= b].
	c2 := c1 copy.
	s2 := c2 size.
	
	c1 removeAll.
	
	self assert: c1 size = 0.
	self assert: c2 size = s2 description: 'the copy has not been modified'.
	
	c1 add: 13; add: 14.
	self assert: (c1 first = 14 and: [c1 second = 13])  description: 'the sortBlock has been preserved'.
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testSort [
	| s |
 	s := SortedCollection withAll: #(3 2 1).
 	self assert: s asArray equals: #(1 2 3).
 	s sort: [:a :b | a >=b]. 
 	self assert: s asArray equals: #(3 2 1).
 	s add: 4.
 	self assert: s asArray equals: #(4 3 2 1).
 	s := SortedCollection new 
 		sortBlock: [:a :b | a >=b];
 		addAll: #(1 2 3);
 		yourself.
 	self assert: s asArray equals: #(3 2 1).
 	s sort.
 	self assert: s asArray equals: #(1 2 3).
 	s add: 0.
 	self assert: s asArray equals: #(0 1 2 3)
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testSortBlock [
		
	|aSortedCollection|
	aSortedCollection := SortedCollection new.
	aSortedCollection sortBlock: [:a :b | a < b].
	aSortedCollection 
		add: 'truite'; 
		add: 'brochet'; 
		add: 'tortue'.
	self assert: aSortedCollection first equals: 'brochet'.
	
	aSortedCollection := SortedCollection new.
	aSortedCollection sortBlock: [:a :b | a >b].
	aSortedCollection 
		add: 'truite'; 
		add: 'brochet';
		add: 'tortue'.
	self assert: aSortedCollection first equals: 'truite'.
	
	
	
]

{ #category : #'tests - basic' }
SortedCollectionTest >> testSpeciesLooseSortBlock [
	"This is a non regression test for http://bugs.squeak.org/view.php?id=6535"

	| numbers reverseOrder firstThree |
	numbers := #(1 2 3 4 5).
	reverseOrder := SortedCollection sortBlock: [:x :y | x > y].
	reverseOrder addAll: numbers.

	"The elements are inverted"
	self assert: (reverseOrder asArray = #(5 4 3 2 1)).

	"Copy the first 3 elements"
	firstThree := reverseOrder copyFrom: 1 to: 3.

	"It appears to work"
	self assert: (firstThree asArray = #(5 4 3)).

	"but we have lost the sort block"
	firstThree add: 1.

	" firstThree is now #(1 5 4 3)! "
	self assert: firstThree asArray = #(5 4 3 1) "fails"
]

{ #category : #requirements }
SortedCollectionTest >> withEqualElements [
	" return a collection of float including equal elements (classic equality)"
	^ collectionOfFloatWithDuplicate
]
