"
These tests are used to assert that FloatArrayPlugin has same results as Float asIEEE32BitWord
"
Class {
	#name : #FloatArrayTest,
	#superclass : #CollectionRootTest,
	#traits : 'TCreationWithTest + TSequencedStructuralEqualityTest + TSequencedConcatenationTest + TSetArithmetic + TAsStringCommaAndDelimiterSequenceableTest + TPrintOnSequencedTest + TBeginsEndsWith + TCloneTest + (TConvertTest - {#testAsByteArray. #integerCollectionWithoutEqualElements}) + TConvertAsSortedTest + (TConvertAsSetForMultiplinessIdentityTest - {#testAsIdentitySetWithEqualsElements. #testAsIdentitySetWithIdentityEqualsElements. #testAsIdentitySetWithoutIdentityEqualsElements. #test0FixtureAsSetForIdentityMultiplinessTest}) + TCopyPartOfSequenceable + TCopyPartOfSequenceableForMultipliness + TCopySequenceableSameContents + TCopySequenceableWithOrWithoutSpecificElements + TCopySequenceableWithReplacement + TCopyTest + (TIncludesWithIdentityCheckTest - {#testIdentityIncludesNonSpecificComportement. #testIdentityIncludes. #test0FixtureIncludeWithIdentityTest}) + (TIndexAccess - {#testIdentityIndexOf. #testIdentityIndexOfIAbsent}) + (TIndexAccessForMultipliness - {#testIdentityIndexOfIAbsentDuplicate. #testIdentityIndexOfDuplicate}) + TIterateSequencedReadableTest + TPutTest + TPutBasicTest + TReplacementSequencedTest + TSequencedElementAccessTest + TSortTest + TSubCollectionAccess',
	#classTraits : 'TCreationWithTest classTrait + TSequencedStructuralEqualityTest classTrait + TSequencedConcatenationTest classTrait + TSetArithmetic classTrait + TAsStringCommaAndDelimiterSequenceableTest classTrait + TPrintOnSequencedTest classTrait + TBeginsEndsWith classTrait + TCloneTest classTrait + TConvertTest classTrait + TConvertAsSortedTest classTrait + TConvertAsSetForMultiplinessIdentityTest classTrait + TCopyPartOfSequenceable classTrait + TCopyPartOfSequenceableForMultipliness classTrait + TCopySequenceableSameContents classTrait + TCopySequenceableWithOrWithoutSpecificElements classTrait + TCopySequenceableWithReplacement classTrait + TCopyTest classTrait + TIncludesWithIdentityCheckTest classTrait + TIndexAccess classTrait + TIndexAccessForMultipliness classTrait + TIterateSequencedReadableTest classTrait + TPutTest classTrait + TPutBasicTest classTrait + TReplacementSequencedTest classTrait + TSequencedElementAccessTest classTrait + TSortTest classTrait + TSubCollectionAccess classTrait',
	#instVars : [
		'nonEmpty5ElementsNoDuplicate',
		'empty',
		'elementNotIn',
		'elementTwiceIn',
		'collectionWithEqualElements',
		'nonEmpty1Element',
		'collectionWithSameAtEndAndBegining',
		'collectionWith1TimeSubcollection',
		'collectionWith2TimeSubcollection',
		'collectionNotIncluded',
		'nonEmptySubcollection',
		'elementInNonEmpty',
		'replacementCollectionSameSize',
		'sortedCollection'
	],
	#category : #'Collections-Tests-Arrayed'
}

{ #category : #requirements }
FloatArrayTest >> aValue [
" return a value to put into nonEmpty"
	^ elementNotIn
]

{ #category : #requirements }
FloatArrayTest >> anIndex [
" return an index in nonEmpty bounds"
	^ 2
]

{ #category : #requirements }
FloatArrayTest >> anotherElementNotIn [
" return an element different of 'elementNotIn'  not included in 'nonEmpty' "
	^ elementNotIn
]

{ #category : #requirements }
FloatArrayTest >> anotherElementOrAssociationIn [
	" return an element (or an association for Dictionary ) present  in 'collection' "
	^ self collection anyOne
]

{ #category : #requirements }
FloatArrayTest >> anotherElementOrAssociationNotIn [
	" return an element (or an association for Dictionary )not present  in 'collection' "
	^ elementNotIn
]

{ #category : #requirements }
FloatArrayTest >> anotherValue [
" return a value ( not eual to 'aValue' ) to put into nonEmpty "
	^ elementInNonEmpty
]

{ #category : #coverage }
FloatArrayTest >> classToBeTested [
	
	^ FloatArray
]

{ #category : #requirements }
FloatArrayTest >> collection [

	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> collectionClass [ 
	"Return the class to be used to create instances of the class tested"
	
	^ FloatArray
]

{ #category : #requirements }
FloatArrayTest >> collectionMoreThan1NoDuplicates [
	" return a collection of size > 1 without equal elements"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> collectionMoreThan5Elements [
" return a collection including at least 5 elements"
	
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> collectionNotIncluded [
" return a collection for wich each element is not included in 'nonEmpty' "
	^ collectionNotIncluded 
		ifNil: [ collectionNotIncluded := (FloatArray new: 2) at:1 put: elementNotIn ; at: 2 put: elementNotIn  ; yourself ].
]

{ #category : #requirements }
FloatArrayTest >> collectionWith1TimeSubcollection [
" return a collection including 'oldSubCollection'  only one time "
	^ collectionWith1TimeSubcollection ifNil: [ collectionWith1TimeSubcollection := collectionWithSameAtEndAndBegining  , self oldSubCollection , collectionWithSameAtEndAndBegining  ].
]

{ #category : #requirements }
FloatArrayTest >> collectionWith2TimeSubcollection [
" return a collection including 'oldSubCollection'  two or many time "
	^ collectionWith2TimeSubcollection ifNil: [ collectionWith2TimeSubcollection := self collectionWith1TimeSubcollection, self oldSubCollection  ].
]

{ #category : #requirements }
FloatArrayTest >> collectionWithCopyNonIdentical [
	" return a collection that include elements for which 'copy' return a different object (this is not the case of SmallInteger)"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> collectionWithElementsToRemove [
" return a collection of elements included in 'nonEmpty'  "
	^ nonEmptySubcollection 
	ifNil: [ nonEmptySubcollection := (FloatArray new:2 ) at:1 put: self nonEmpty first ; at:2 put: self nonEmpty last ; yourself ]
]

{ #category : #requirements }
FloatArrayTest >> collectionWithNonIdentitySameAtEndAndBegining [
	" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	^ collectionWithSameAtEndAndBegining
]

{ #category : #requirements }
FloatArrayTest >> collectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	^ collectionWithSameAtEndAndBegining
]

{ #category : #requirements }
FloatArrayTest >> collectionWithSortableElements [
" return a collection elements that can be sorte ( understanding message ' < '  or ' > ')"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> collectionWithoutEqualElements [

" return a collection not including equal elements "
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> elementInForElementAccessing [
" return an element inculded in 'moreThan4Elements'"
	^ elementInNonEmpty
]

{ #category : #requirements }
FloatArrayTest >> elementInForIndexAccessing [
" return an element included in 'collectionMoreThan1NoDuplicates' "
	^ elementInNonEmpty .
]

{ #category : #requirements }
FloatArrayTest >> elementNotIn [
"return an element not included in 'nonEmpty' "

	^ elementNotIn
]

{ #category : #requirements }
FloatArrayTest >> elementNotInForElementAccessing [
" return an element not included in 'moreThan4Elements' "
	^ elementNotIn
]

{ #category : #requirements }
FloatArrayTest >> elementNotInForIndexAccessing [
" return an element not included in 'collectionMoreThan1NoDuplicates' "
	^ elementNotIn
]

{ #category : #requirements }
FloatArrayTest >> elementToAdd [
" return an element of type 'nonEmpy' elements'type'  not  yet included in nonEmpty" 
	^ elementNotIn
]

{ #category : #requirements }
FloatArrayTest >> elementsCopyNonIdenticalWithoutEqualElements [
	" return a collection that does niot incllude equal elements ( classic equality )
	all elements included are elements for which copy is not identical to the element  "
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> empty [
	^ empty
]

{ #category : #requirements }
FloatArrayTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> firstIndex [
" return an index between 'nonEmpty' bounds that is < to 'second index' "
	^2
]

{ #category : #'tests - copying with replacement' }
FloatArrayTest >> firstIndexesOf: aSubCollection in: collection [
" return an OrderedCollection with the first indexes of the occurrences of subCollection in  collection "
	| tmp result currentIndex |
	tmp:= collection.
	result:= OrderedCollection new.
	currentIndex := 1.
	
	[tmp isEmpty ]whileFalse:
		[
		(tmp beginsWith: aSubCollection)
			ifTrue: [ 	
				result add: currentIndex.
				1 to: aSubCollection size do: 
					[:i | 
					tmp := tmp copyWithoutFirst.
					currentIndex := currentIndex + 1]
				]
			ifFalse: [
				tmp := tmp copyWithoutFirst.
				currentIndex := currentIndex +1.
				]
		 ].
	
	^ result.
]

{ #category : #'tests - fixture' }
FloatArrayTest >> howMany: aSubCollection in: collection [
" return an integer representing how many time 'subCollection'  appears in 'collection'  "
	| tmp nTime |
	tmp := collection.
	nTime:= 0.
	
	[tmp isEmpty ]whileFalse:
		[
		(tmp beginsWith: aSubCollection)
			ifTrue: [ 	
				nTime := nTime + 1.
				1 to: aSubCollection size do: [:i | tmp := tmp copyWithoutFirst.]
				]
			ifFalse: [tmp := tmp copyWithoutFirst.]
		 ].
	
	^ nTime.
]

{ #category : #requirements }
FloatArrayTest >> indexArray [
" return a Collection including indexes between bounds of 'nonEmpty' "

	^ { 1. 4. 3.}
]

{ #category : #requirements }
FloatArrayTest >> indexInForCollectionWithoutDuplicates [
" return an index between 'collectionWithoutEqualsElements'  bounds"
	^ 2.
]

{ #category : #requirements }
FloatArrayTest >> indexInNonEmpty [
	"Return an index between bounds of 'nonEmpty'"

	^ 3
]

{ #category : #requirements }
FloatArrayTest >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> newElement [
"return an element that will be put in the collection in place of another"
	^ elementNotIn
]

{ #category : #requirements }
FloatArrayTest >> nonEmpty [

	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> nonEmpty1Element [
" return a collection of size 1 including one element"
	^ nonEmpty1Element
]

{ #category : #requirements }
FloatArrayTest >> nonEmptyMoreThan1Element [
" return a collection that doesn't includes equal elements' and doesn't include nil elements'" 
	^nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> oldSubCollection [
" return a subCollection included in collectionWith1TimeSubcollection .
ex :   subCollection := #( 2 3 4) and collectionWith1TimeSubcollection := #(1 2 3 4 5)"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> replacementCollection [
" return a collection that will be used to replace 'oldSubcollection' in ' collectionWith1TimeSubcollection'  " 
	^ collectionWithSameAtEndAndBegining
]

{ #category : #requirements }
FloatArrayTest >> replacementCollectionSameSize [
" return a collection of size (secondIndex - firstIndex + 1)"
	^replacementCollectionSameSize 
		ifNil: [ 	replacementCollectionSameSize := FloatArray new: (self secondIndex  - self firstIndex  + 1).
				 1 to: replacementCollectionSameSize size do:
					[ :i | replacementCollectionSameSize at:i put: elementInNonEmpty ].
				replacementCollectionSameSize.
				 ].
]

{ #category : #requirements }
FloatArrayTest >> secondCollection [
" return a collection that will be the second part of the concatenation"
	^ collectionWithEqualElements
]

{ #category : #requirements }
FloatArrayTest >> secondIndex [
" return an index between 'nonEmpty' bounds that is > to 'first index' "
	^self firstIndex +1
]

{ #category : #running }
FloatArrayTest >> setUp [
	super setUp.
	empty := FloatArray new.
	elementInNonEmpty := 7.0.
	nonEmpty5ElementsNoDuplicate := (FloatArray new: 5)
		at: 1 put: 1.5;
		at: 2 put: 2.5;
		at: 3 put: elementInNonEmpty;
		at: 4 put: 4.5;
		at: 5 put: 5.5;
		yourself.
	elementNotIn := 999.0.
	elementTwiceIn := 2.3.
	collectionWithEqualElements := (FloatArray new: 3)
		at: 1 put: 2.0;
		at: 2 put: 2.0;
		at: 3 put: 3.5;
		yourself.
	nonEmpty1Element := (FloatArray new: 1)
		at: 1 put: 1.2;
		yourself.
	collectionWithSameAtEndAndBegining := (FloatArray new: 3)
		at: 1 put: 2.0;
		at: 2 put: 1.0;
		at: 3 put: 2.0 copy;
		yourself
]

{ #category : #requirements }
FloatArrayTest >> sizeCollection [
	"Answers a collection not empty"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> sortedInAscendingOrderCollection [
	"Return a collection sorted in ascending order"
	^ sortedCollection ifNil: [ sortedCollection := ( FloatArray new: 3)at: 1 put: 1.0 ; at: 2 put: 2.0 ; at: 3 put: 3.0 ; yourself ]
]

{ #category : #requirements }
FloatArrayTest >> subCollectionNotIn [
" return a collection for which at least one element is not included in 'moreThan4Elements' "
	^ collectionNotIncluded 
		ifNil: [ collectionNotIncluded := (FloatArray new: 2) at:1 put: elementNotIn ; at: 2 put: elementNotIn  ; yourself ].
]

{ #category : #'tests - arithmetic' }
FloatArrayTest >> testAddNoMutation [
	"this test checks that addition does not impact the orginal arrays"

	| a b c |
	a := { 1.2. 2.3 } asFloatArray.
	b := { 1.2. 2.3 } asFloatArray.
	c := a + b.
	self assert: a equals: { 1.2. 2.3 } asFloatArray.
	self assert: b equals: { 1.2. 2.3 } asFloatArray
]

{ #category : #'tests - arithmetic' }
FloatArrayTest >> testArithmeticCoercion [
	"This test is related to http://bugs.squeak.org/view.php?id=6782"
	
	self assert: 3.0 / (FloatArray with: 2.0) equals: (FloatArray with: 1.5).
	self assert: 3.0 * (FloatArray with: 2.0) equals: (FloatArray with: 6.0).
	self assert: 3.0 + (FloatArray with: 2.0) equals: (FloatArray with: 5.0).
	self assert: 3.0 - (FloatArray with: 2.0) equals: (FloatArray with: 1.0).
	self assert: 3.0 / (FloatArray with: 2.0) equals: (FloatArray with: 1.5).
	self assert: 3.0 * (FloatArray with: 2.0) equals: (FloatArray with: 6.0).
	self assert: 3.0 + (FloatArray with: 2.0) equals: (FloatArray with: 5.0).
	self assert: 3.0 - (FloatArray with: 2.0) equals: (FloatArray with: 1.0)
]

{ #category : #'tests - as string comma delimiter sequenceable' }
FloatArrayTest >> testAsCommaStringEmpty [

	self assert: self empty asCommaString = ''.
	self assert: self empty asCommaStringAnd = ''
]

{ #category : #'tests - comma and delimiter' }
FloatArrayTest >> testAsCommaStringOne [
	
	"self assert: self oneItemCol asCommaString = '1'.
	self assert: self oneItemCol asCommaStringAnd = '1'."

	self assert: self nonEmpty1Element asCommaString = (self nonEmpty1Element first asString).
	self assert: self nonEmpty1Element asCommaStringAnd = (self nonEmpty1Element first asString)
]

{ #category : #'tests - converting' }
FloatArrayTest >> testAsIdentitySet [
	"Test with a collection without equal elements :"

	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: IdentitySet
]

{ #category : #'tests - converting' }
FloatArrayTest >> testAsSet [
 	"Test with a collection without equal elements"
	
	self assertSameContents: self collectionWithoutEqualElements whenConvertedTo: Set
]

{ #category : #'tests - as sorted collection' }
FloatArrayTest >> testAsSortedCollectionWithSortBlock [

	| result tmp |
	result := self collectionWithSortableElements  asSortedCollection: [:a :b | a > b ].
	self assert: (result class includesBehavior: SortedCollection).
	result do: 
		[ :each | 
		self assert: (self collectionWithSortableElements occurrencesOf: each) equals: (result occurrencesOf: each) ].
	self assert: result size equals: self collectionWithSortableElements  size.
	tmp:=result at: 1.
	result do: [:each| self assert: tmp >= each. tmp := each ]
]

{ #category : #'tests - comma and delimiter' }
FloatArrayTest >> testAsStringOnDelimiterLastMore [

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

{ #category : #'tests - comma and delimiter' }
FloatArrayTest >> testAsStringOnDelimiterLastOne [

	| delim oneItemStream result |	
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]

{ #category : #'tests - comma and delimiter' }
FloatArrayTest >> testAsStringOnDelimiterOne [

	| delim oneItemStream result |
	delim := ', '.
	result := ''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]

{ #category : #'tests - puting with indexes' }
FloatArrayTest >> testAtAllIndexesPut [
	
	self nonEmpty atAllPut: self aValue.
	self nonEmpty do:[ :each| self assert: each = self aValue].
]

{ #category : #'tests - puting with indexes' }
FloatArrayTest >> testAtAllPut [
 
	self nonEmpty atAll: self indexArray put: self aValue.
	
	self indexArray do:
		[:i | self assert: (self nonEmpty at: i) equals: self aValue ]
]

{ #category : #'tests - at put' }
FloatArrayTest >> testAtPut [
	
	self nonEmpty at: self anIndex put: self aValue.
	self assert: (self nonEmpty at: self anIndex) = self aValue.
]

{ #category : #'tests - at put' }
FloatArrayTest >> testAtPutOutOfBounds [
	
	self should: [self empty at: self anIndex put: self aValue] raise: Error
]

{ #category : #'tests - copy - clone' }
FloatArrayTest >> testCopyCreatesNewObject [
	 
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: self nonEmpty == copy.
]

{ #category : #'tests - copy' }
FloatArrayTest >> testCopyNonEmptyWithout [
	
	| res anElementOfTheCollection |
	anElementOfTheCollection :=  self nonEmpty anyOne.
	res := (self nonEmpty copyWithout: anElementOfTheCollection).
	"here we do not test the size since for a non empty set we would get a problem.
	Then in addition copy is not about duplicate management. The element should 
	be in at the end."
	self deny: (res includes: anElementOfTheCollection).
	self nonEmpty do:
		[:each | (each = anElementOfTheCollection) 
					ifFalse: [self assert: (res includes: each)]]
]

{ #category : #'tests - copying with replacement' }
FloatArrayTest >> testCopyReplaceAllWith1Occurrence [
	| result  firstIndexesOfOccurrence index endPartIndexResult endPartIndexCollection |
	
	result := self collectionWith1TimeSubcollection  copyReplaceAll: self oldSubCollection with: self replacementCollection .
	
	"detecting indexes of olSubCollection"
	firstIndexesOfOccurrence  := self firstIndexesOf: self oldSubCollection in: self collectionWith1TimeSubcollection .
	index:= firstIndexesOfOccurrence at: 1.
	
	"verify content of 'result' : "
	"first part of 'result'' : '"

	1 to: (index -1) do: 
		[
		:i |  
		self assert: (self collectionWith1TimeSubcollection  at:i)=(result at: i)
		].

	" middle part containing replacementCollection : "
	
	index to: (index + self replacementCollection size-1) do: 
		[
		:i |
		self assert: ( result at: i )=(self replacementCollection at: ( i - index + 1 ))
		].
	
	" end part :"
	
	endPartIndexResult :=  index + self replacementCollection  size .
	endPartIndexCollection :=   index + self oldSubCollection size  .
	
	1 to: (result size - endPartIndexResult - 1 ) do:
		[ 
		:i |
		self assert: (result at: ( endPartIndexResult + i - 1 ) ) = (self collectionWith1TimeSubcollection  at: ( endPartIndexCollection + i - 1 ) ).
		].
]

{ #category : #'tests - copying with replacement' }
FloatArrayTest >> testCopyReplaceFromToWith [
	| result  indexOfSubcollection lastIndexOfOldSubcollection lastIndexOfReplacementCollection |
	
	indexOfSubcollection := (self firstIndexesOf: self oldSubCollection  in:  self collectionWith1TimeSubcollection) at: 1. 
	lastIndexOfOldSubcollection := indexOfSubcollection + self oldSubCollection size -1.
	lastIndexOfReplacementCollection := indexOfSubcollection + self replacementCollection  size -1.
	
	result := self collectionWith1TimeSubcollection  copyReplaceFrom: indexOfSubcollection  to: lastIndexOfOldSubcollection   with: self replacementCollection .
	
	"verify content of 'result' : "
	"first part of 'result'  "
	
	1 to: (indexOfSubcollection  - 1) do: 
		[ 
		:i | 
		self assert: (self collectionWith1TimeSubcollection  at:i) = (result at: i)
		].
	
	" middle part containing replacementCollection : "
	
	(indexOfSubcollection ) to: ( lastIndexOfReplacementCollection  ) do: 
		[
		:i |
		self assert: (result at: i)=(self replacementCollection at: (i - indexOfSubcollection +1))
		].
	
	" end part :"
	1 to: (result size - lastIndexOfReplacementCollection   ) do:
		[ 
		:i |
		self assert: (result at: ( lastIndexOfReplacementCollection  + i  ) ) = (self collectionWith1TimeSubcollection  at: ( lastIndexOfOldSubcollection  + i  ) ).
		].
]

{ #category : #'tests - copying with replacement' }
FloatArrayTest >> testCopyReplaceFromToWithInsertion [
	| result  indexOfSubcollection |
	
	indexOfSubcollection := (self firstIndexesOf: self oldSubCollection  in:  self collectionWith1TimeSubcollection) at: 1. 
	
	result := self collectionWith1TimeSubcollection  copyReplaceFrom: indexOfSubcollection  to: ( indexOfSubcollection - 1 ) with: self replacementCollection .
	
	"verify content of 'result' : "
	"first part of 'result'' : '"
	
	1 to: (indexOfSubcollection -1) do: 
		[
		:i | 
		self assert: (self collectionWith1TimeSubcollection  at:i)=(result at: i)
		].
	
	" middle part containing replacementCollection : "
	indexOfSubcollection  to: (indexOfSubcollection  + self replacementCollection size-1) do: 
		[
		:i |
		self assert: ( result at: i )=(self replacementCollection at: ( i - indexOfSubcollection +1 ))
		].
	
	" end part :"
	(indexOfSubcollection  + self replacementCollection size) to: (result size) do:
		[:i|
		self assert: (result at: i)=(self collectionWith1TimeSubcollection  at: (i-self replacementCollection size))].
	
	" verify size: "	
	self assert: result size=(self collectionWith1TimeSubcollection  size + self replacementCollection size).
]

{ #category : #'tests - copying part of sequenceable' }
FloatArrayTest >> testCopyUpTo [

	| result index collection |
	collection := self collectionWithoutEqualElements.
	index:= self indexInForCollectionWithoutDuplicates.
	result := collection   copyUpTo: (collection  at:index).
	
	"Verify content"
	result withIndexDo: [:el :i| self assert: (collection at:i) equals: (result at:i)].
	
	"Verify size"
	self assert: result size equals: (index-1)
]

{ #category : #'tests - copying part of sequenceable for multipliness' }
FloatArrayTest >> testCopyUpToLastWithDuplicate [

	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.
	
	"collectionWithSameAtEndAndBegining first and last elements are equals.
	'copyUpToLast:' should copy until the last occurrence :"
	result := collection copyUpToLast: (element ).
	
	"Verify content"
	result withIndexDo: 
		[:el :i | self assert: (result at: i) equals: (collection at: i) ].
	
	self assert: result size equals: (collection size - 1)
]

{ #category : #'tests - iterate on sequenced reable collections' }
FloatArrayTest >> testDetectSequenced [
	"Testing that detect keep the first element returning true for sequenceable collections"

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element  detect: [:each | each notNil ].
	self assert: result equals: element
]

{ #category : #'tests - arithmetic' }
FloatArrayTest >> testDotProduct [
	| a b |
	a := #(2.0 3.0 4.0 5.0) asFloatArray.
	b := #(2.0 3.0 4.0 5.0) asFloatArray.
	self assert: (a dot: b) equals: 54
]

{ #category : #'tests - equality' }
FloatArrayTest >> testEqualSignIsTrueForEmptyButNonIdenticalCollections [
	
	self assert: self empty equals: self empty copy. 
	self assert: self empty copy equals: self empty.
	self assert: self empty copy equals: self empty copy
]

{ #category : #'tests - equality' }
FloatArrayTest >> testEqualSignOfIdenticalCollectionObjects [
	
	self assert: self empty equals: self empty. 
	self assert: self nonEmpty equals: self nonEmpty
]

{ #category : #'tests - iterate on sequenced reable collections' }
FloatArrayTest >> testFindFirst [

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element findFirst: [:each | each = element].
	
	self assert: result equals: 1
]

{ #category : #testing }
FloatArrayTest >> testFloatArrayPluginPrimitiveAt [
	"if FloatArrayPlugin primitive are not here, this test is dumb.
	Otherwise, it will compare primitive and #fromIEEE32Bit:"

	#(2r0.0 2r1.0 2r1.1 2r1.00000000000000000000001 2r1.0e-10 2r1.1e-10 2r1.00000000000000000000001e-10 2r1.0e10 2r1.1e10 2r1.00000000000000000000001e10 2r1.0e-126 2r1.11111111111111111111111e127 2r1.11111111111111111111111e128 2r0.11111111111111111111111e-126 2r0.00000000000000000000001e-126 2r1.0000000000000000000000001 2r1.0000000000000000000000010 2r1.0000000000000000000000011 2r1.0000000000000000000000100 2r1.0000000000000000000000101 2r1.0000000000000000000000110 2r1.0000000000000000000000111 2r1.1111111111111111111111001 2r1.1111111111111111111111010 2r1.1111111111111111111111011 2r1.1111111111111111111111101 2r1.1111111111111111111111110 2r1.1111111111111111111111111 2r1.1111111111111111111111110e127 2r0.1111111111111111111111111e-126 2r0.1111111111111111111111110e-126 2r0.1111111111111111111111101e-126 2r0.1111111111111111111111011e-126 2r0.1111111111111111111111010e-126 2r0.1111111111111111111111001e-126 2r0.0000000000000000000000111e-126 2r0.0000000000000000000000110e-126 2r0.0000000000000000000000101e-126 2r0.0000000000000000000000011e-126 2r0.0000000000000000000000010e-126 2r0.0000000000000000000000001e-126 2r0.0000000000000000000000010000000000000000000000000001e-126)
		do: [ :e | 
			self assert: ((FloatArray with: e) at: 1) equals: (Float fromIEEE32Bit: ((FloatArray with: e) basicAt: 1)).
			self assert: ((FloatArray with: e negated) at: 1) equals: (Float fromIEEE32Bit: ((FloatArray with: e negated) basicAt: 1)) ].
	"regular numbers no truncation or rounding"

	"smallest float32 before gradual underflow"

	"biggest float32"

	"overflow"

	"gradual underflow"

	"with rounding mode : tests on 25 bits"

	"overflow"

	"gradual underflow"

	"special cases"
	(Array with: Float infinity with: Float infinity negated with: Float negativeZero)
		do: [ :e | self assert: ((FloatArray with: e) at: 1) equals: (Float fromIEEE32Bit: ((FloatArray with: e) basicAt: 1)) ].

	"Cannot compare NaN"
	(Array with: Float nan) do: [ :e | self assert: (Float fromIEEE32Bit: ((FloatArray with: e) basicAt: 1)) isNaN ]
]

{ #category : #testing }
FloatArrayTest >> testFloatArrayPluginPrimitiveAtPut [
	"if FloatArrayPlugin primitive are not here, this test is dumb.
	Otherwise, it will compare primitive and #asIEEE32BitWord"

	#(2r0.0 2r1.0 2r1.1 2r1.00000000000000000000001 2r1.0e-10 2r1.1e-10 2r1.00000000000000000000001e-10 2r1.0e10 2r1.1e10 2r1.00000000000000000000001e10 2r1.0e-126 2r1.11111111111111111111111e127 2r1.11111111111111111111111e128 2r0.11111111111111111111111e-126 2r0.00000000000000000000001e-126 2r1.0000000000000000000000001 2r1.0000000000000000000000010 2r1.0000000000000000000000011 2r1.0000000000000000000000100 2r1.0000000000000000000000101 2r1.0000000000000000000000110 2r1.0000000000000000000000111 2r1.1111111111111111111111001 2r1.1111111111111111111111010 2r1.1111111111111111111111011 2r1.1111111111111111111111101 2r1.1111111111111111111111110 2r1.1111111111111111111111111 2r1.1111111111111111111111110e127 2r0.1111111111111111111111111e-126 2r0.1111111111111111111111110e-126 2r0.1111111111111111111111101e-126 2r0.1111111111111111111111011e-126 2r0.1111111111111111111111010e-126 2r0.1111111111111111111111001e-126 2r0.0000000000000000000000111e-126 2r0.0000000000000000000000110e-126 2r0.0000000000000000000000101e-126 2r0.0000000000000000000000011e-126 2r0.0000000000000000000000010e-126 2r0.0000000000000000000000001e-126 2r0.0000000000000000000000010000000000000000000000000001e-126)
		do: [ :e | 
			self assert: ((FloatArray with: e) basicAt: 1) equals: e asIEEE32BitWord.
			self assert: ((FloatArray with: e negated) basicAt: 1) equals: e negated asIEEE32BitWord ].
	"regular numbers no truncation or rounding"

	"smallest float32 before gradual underflow"

	"biggest float32"

	"overflow"

	"gradual underflow"

	"with rounding mode : tests on 25 bits"

	"overflow"

	"gradual underflow"

	"special cases"
	(Array
		with: Float infinity
		with: Float infinity negated
		with: Float negativeZero
		with: Float nan) do: [ :e | self assert: ((FloatArray with: e) basicAt: 1) equals: e asIEEE32BitWord ]
]

{ #category : #'tests - equality' }
FloatArrayTest >> testHasEqualElementsIsTrueForNonIdenticalButEqualCollections [
		
	self assert: (self empty hasEqualElements: self empty copy). 
	self assert: (self empty copy hasEqualElements: self empty).
	self assert: (self empty copy hasEqualElements: self empty copy).
		
	self assert: (self nonEmpty hasEqualElements: self nonEmpty copy). 
	self assert: (self nonEmpty copy hasEqualElements: self nonEmpty).
	self assert: (self nonEmpty copy hasEqualElements: self nonEmpty copy)
]

{ #category : #'tests - equality' }
FloatArrayTest >> testHasEqualElementsOfIdenticalCollectionObjects [
	
	self assert: (self empty hasEqualElements: self empty). 
	self assert: (self nonEmpty hasEqualElements: self nonEmpty)
]

{ #category : #'tests - set arithmetic' }
FloatArrayTest >> testIntersectionItself [
	
	| result |
	result := self collectionWithoutEqualElements intersection: self collectionWithoutEqualElements.
	self assert: result size equals: self collectionWithoutEqualElements size.
	self collectionWithoutEqualElements do: [ :each|
		self assert: (result includes: each) ].
]

{ #category : #'tests - sorting' }
FloatArrayTest >> testIsSortedBy [
	self assert: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a<b]).
	self deny: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a>b]).
]

{ #category : #'tests - iterate on sequenced reable collections' }
FloatArrayTest >> testKeysAndValuesDo [
	"| result |
	result:= OrderedCollection new.
	
	self nonEmptyMoreThan1Element  keysAndValuesDo: 
		[:i :value|
		result add: (value+i)].
	
	1 to: result size do:
		[:i|
		self assert: (result at:i)=((self nonEmptyMoreThan1Element at:i)+i)]"
	|  indexes elements |
	indexes := OrderedCollection new.
	elements := OrderedCollection new.
	
	self nonEmptyMoreThan1Element  keysAndValuesDo: 
		[:i :value|
		indexes  add: (i).
		elements add: value].
	
	(1 to: self nonEmptyMoreThan1Element size )do:
		[ :i |
		self assert: (indexes at: i) equals: i.
		self assert: (elements at: i) equals: (self nonEmptyMoreThan1Element at: i).	
		].
	
	self assert: indexes size equals: elements size. 
	self assert: indexes size equals: self nonEmptyMoreThan1Element size
]

{ #category : #'tests - creation' }
FloatArrayTest >> testOfSize [
	
	| aCol |
	aCol := self collectionClass ofSize: 3.
	self assert: aCol size equals: 3.
]

{ #category : #'tests - iterate on sequenced reable collections' }
FloatArrayTest >> testReverseWithDo [
	
	| firstCollection secondCollection index |

	firstCollection := self nonEmptyMoreThan1Element.
	secondCollection := firstCollection copy.
	index := firstCollection size.
	
	firstCollection  reverseWith: secondCollection do:
		[:a :b |
	
		self assert: (firstCollection at: index) equals: a. 
		self assert: (secondCollection at: index) equals: b.
			(index := index - 1)]
]

{ #category : #tests }
FloatArrayTest >> testSumWithFloatScalar [
	| floatArray scalar expectedResult actualResult |
	
	floatArray := #(1 2) asFloatArray.
	scalar := 0.1.
	
	expectedResult := #(1.1 2.1) asFloatArray.
	actualResult := floatArray + scalar.
	
	self assert: actualResult equals: expectedResult.
]

{ #category : #tests }
FloatArrayTest >> testSumWithIntegerArray [
	| floatArray integerArray expectedResult actualResult |
	
	floatArray := #(1 2) asFloatArray.
	integerArray := #(3 4) asIntegerArray.
	
	expectedResult := #(4 6) asFloatArray.
	actualResult := floatArray + integerArray.
	
	self assert: actualResult equals: expectedResult.
]

{ #category : #tests }
FloatArrayTest >> testSumWithIntegerScalar [
	| floatArray scalar expectedResult actualResult |
	
	floatArray := #(1 2) asFloatArray.
	scalar := 3.
	
	expectedResult := #(4 5) asFloatArray.
	actualResult := floatArray + scalar.
	
	self assert: actualResult equals: expectedResult.
]

{ #category : #tests }
FloatArrayTest >> testSumWithOrderedCollection [
	| floatArray collection expectedResult actualResult |
	
	floatArray := #(1 2) asFloatArray.
	collection := #(3 4) asOrderedCollection.
	
	expectedResult := #(4 6) asFloatArray.
	actualResult := floatArray + collection.
	
	self assert: actualResult equals: expectedResult.
]

{ #category : #'tests - puting with indexes' }
FloatArrayTest >> testSwapWith [
 
	| result index |
	index := self indexArray anyOne.
	result:= self nonEmpty copy .
	result swap: index with: 1.
	self assert: (result at: index) = (self nonEmpty at:1).
	self assert: (result at: 1) = (self nonEmpty at: index).
]

{ #category : #'tests - iterate on sequenced reable collections' }
FloatArrayTest >> testWithDo [
	
	| firstCollection secondCollection index |

	firstCollection := self nonEmptyMoreThan1Element.
	secondCollection := firstCollection copy.
	index := 0.
	
	firstCollection  with: secondCollection do:
		[:a :b |
		index := index + 1.
		self assert: (firstCollection at: index) equals:  a. 
		self assert: (secondCollection at: index) equals: b ]
]

{ #category : #'tests - iterate on sequenced reable collections' }
FloatArrayTest >> testWithIndexCollect [
	
	| result index collection |
	index := 0.
	collection := self nonEmptyMoreThan1Element .
	result := collection  withIndexCollect: [:each :i | 
		self assert: i equals: (index := index + 1).	
		self assert: i equals: (collection  indexOf: each).
		each ]. 
	
	result withIndexDo: [:el :i | self assert: (result at:i) equals: (collection at: i)].
	self assert: result size equals: collection size
]

{ #category : #'tests - iterate on sequenced reable collections' }
FloatArrayTest >> testWithIndexDo [

	|  indexes elements |
	indexes:= OrderedCollection new.
	elements := OrderedCollection new.
	
	self nonEmptyMoreThan1Element withIndexDo: 
		[:value :i  |
			indexes  add: (i).
			elements add: value ].
	
	self nonEmptyMoreThan1Element withIndexDo: 
		[:el :i |
			self assert: (indexes at: i) equals: i.
			self assert: (elements at: i) equals: el ].
	
	self assert: indexes size equals: elements size. 
	self assert: indexes size equals: self nonEmptyMoreThan1Element size
]

{ #category : #'tests - creation' }
FloatArrayTest >> testWithWith [
	
	| aCol collection element1 element2 |
	collection := self collectionMoreThan5Elements asOrderedCollection copyFrom: 1 to: 2.
	element1 := collection at: 1.
	element2 := collection at: 2.
	
	aCol := self collectionClass with: element1  with: element2 .
	self assert: (aCol occurrencesOf: element1 ) equals: (collection occurrencesOf: element1).
	self assert: (aCol occurrencesOf: element2 ) equals: (collection occurrencesOf: element2)
]

{ #category : #'tests - begins ends with' }
FloatArrayTest >> testsBeginsWithEmpty [
	
	self deny: (self nonEmpty beginsWith:(self empty)).
	self deny: (self empty beginsWith:(self nonEmpty))
]

{ #category : #'tests - begins ends with' }
FloatArrayTest >> testsEndsWithEmpty [
	
	self deny: (self nonEmpty endsWith: self empty).
	self deny: (self empty endsWith: self nonEmpty)
]

{ #category : #requirements }
FloatArrayTest >> unsortedCollection [
" retur a collection that is not yat sorted"
	^nonEmpty5ElementsNoDuplicate
]

{ #category : #requirements }
FloatArrayTest >> withEqualElements [
	^ collectionWithEqualElements
]
