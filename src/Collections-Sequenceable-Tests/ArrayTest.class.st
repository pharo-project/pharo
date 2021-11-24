"
This is the unit test for the class Array. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org/)	
	- the sunit class category
"
Class {
	#name : #ArrayTest,
	#superclass : #CollectionRootTest,
	#traits : 'TEmptySequenceableTest + TSequencedElementAccessTest + TCloneTest + TIncludesWithIdentityCheckTest + TCopyTest + TCreationWithTest + TPutBasicTest + TConvertTest + TOccurrencesForMultiplinessTest + TIterateSequencedReadableTest + TSequencedConcatenationTest + TReplacementSequencedTest + TAsStringCommaAndDelimiterSequenceableTest + TBeginsEndsWith + TPrintOnSequencedTest + TIndexAccess + TSubCollectionAccess + TConvertAsSetForMultiplinessIdentityTest + TCopyPartOfSequenceable + TCopySequenceableSameContents + TCopySequenceableWithOrWithoutSpecificElements + TCopySequenceableWithReplacement + TIndexAccessForMultipliness + TCopyPartOfSequenceableForMultipliness + TConvertAsSortedTest + TPutTest + TSequencedStructuralEqualityTest + TSortTest + TSetArithmetic + TEnumeratingTest',
	#classTraits : 'TEmptySequenceableTest classTrait + TSequencedElementAccessTest classTrait + TCloneTest classTrait + TIncludesWithIdentityCheckTest classTrait + TCopyTest classTrait + TCreationWithTest classTrait + TPutBasicTest classTrait + TConvertTest classTrait + TOccurrencesForMultiplinessTest classTrait + TIterateSequencedReadableTest classTrait + TSequencedConcatenationTest classTrait + TReplacementSequencedTest classTrait + TAsStringCommaAndDelimiterSequenceableTest classTrait + TBeginsEndsWith classTrait + TPrintOnSequencedTest classTrait + TIndexAccess classTrait + TSubCollectionAccess classTrait + TConvertAsSetForMultiplinessIdentityTest classTrait + TCopyPartOfSequenceable classTrait + TCopySequenceableSameContents classTrait + TCopySequenceableWithOrWithoutSpecificElements classTrait + TCopySequenceableWithReplacement classTrait + TIndexAccessForMultipliness classTrait + TCopyPartOfSequenceableForMultipliness classTrait + TConvertAsSortedTest classTrait + TPutTest classTrait + TSequencedStructuralEqualityTest classTrait + TSortTest classTrait + TSetArithmetic classTrait + TEnumeratingTest classTrait',
	#instVars : [
		'example1',
		'literalArray',
		'selfEvaluatingArray',
		'nonSEArray1',
		'nonSEarray2',
		'example2',
		'empty',
		'collectResult',
		'withoutEqualElements',
		'withEqualElements',
		'withCharacters',
		'unsortedCollection',
		'sortedInAscendingOrderCollection',
		'collectionNotIncluded',
		'removedCollection',
		'elementInForCopy',
		'elementNotInForCopy',
		'firstIndex',
		'secondIndex',
		'replacementCollection',
		'indexArray',
		'valueArray',
		'nonEmptyMoreThan1Element',
		'subCollectionNotIn',
		'replacementCollectionSameSize',
		'oldSubCollection',
		'nonEmpty1Element',
		'collectionOfCollection',
		'collectionOfFloatWithEqualElements',
		'collectionWithoutNil',
		'duplicateElement',
		'collection5Elements',
		'collectionWith4Elements',
		'collectionOfCollectionsOfStrings',
		'collectionOfCollectionsOfInts',
		'simpleCollection',
		'stringCollectionWithSameBeginingAnEnd',
		'collectionOfFloat'
	],
	#category : #'Collections-Sequenceable-Tests-Base'
}

{ #category : #helpers }
ArrayTest >> aValue [

	^ 33
]

{ #category : #requirements }
ArrayTest >> accessCollection [

	^ example1
]

{ #category : #helpers }
ArrayTest >> anIndex [

	^ 2
]

{ #category : #requirements }
ArrayTest >> anotherElementNotIn [
	^ elementNotInForCopy
]

{ #category : #requirements }
ArrayTest >> anotherElementOrAssociationIn [
	" return an element (or an association for Dictionary ) present  in 'collection' "
	^ self collection anyOne.
]

{ #category : #requirements }
ArrayTest >> anotherElementOrAssociationNotIn [
	" return an element (or an association for Dictionary )not present  in 'collection' "
	^ elementNotInForCopy
]

{ #category : #helpers }
ArrayTest >> anotherValue [

	^ 66
]

{ #category : #asserting }
ArrayTest >> assertSameContents: aCollection whenConvertedTo: aClass [
	| result |
	result := aCollection perform: ('as' , aClass name) asSymbol.
	self assert: (result class includesBehavior: aClass).
	result do: [ :each | self assert: (aCollection occurrencesOf: each) equals: (result occurrencesOf: each) ].
	self assert: result size equals: aCollection size
]

{ #category : #coverage }
ArrayTest >> classToBeTested [
	
	^ Array
]

{ #category : #initialization }
ArrayTest >> collection [ 

	^ collectionWith4Elements
]

{ #category : #'tests - creation' }
ArrayTest >> collectionClass [ 
	"Return the class to be used to create instances of the class tested"
		
	^ Array
]

{ #category : #requirements }
ArrayTest >> collectionInForIncluding [
	^ self nonEmpty copyWithoutFirst.
]

{ #category : #requirements }
ArrayTest >> collectionMoreThan1NoDuplicates [
	" return a collection of size 5 without equal elements"
	^ withoutEqualElements
]

{ #category : #requirements }
ArrayTest >> collectionMoreThan5Elements [
" return a collection including at least 5 elements"
	
	^ collection5Elements
]

{ #category : #requirements }
ArrayTest >> collectionNotIncluded [
	^ collectionNotIncluded.
]

{ #category : #requirements }
ArrayTest >> collectionOfCollectionsOfInts [
	^collectionOfCollectionsOfInts.
]

{ #category : #requirements }
ArrayTest >> collectionOfCollectionsOfStrings [
	^ collectionOfCollectionsOfStrings.
]

{ #category : #requirements }
ArrayTest >> collectionOfFloat [
	^ collectionOfFloat
]

{ #category : #requirements }
ArrayTest >> collectionWith1TimeSubcollection [
	^ (self oldSubCollection copyWithoutFirst),self oldSubCollection,(self oldSubCollection copyWithoutFirst).
]

{ #category : #requirements }
ArrayTest >> collectionWith2TimeSubcollection [
	^ (self oldSubCollection copyWithoutFirst),self oldSubCollection,(self oldSubCollection copyWithoutFirst),self oldSubCollection .
]

{ #category : #requirements }
ArrayTest >> collectionWithCharacters [
	^ withCharacters.
]

{ #category : #requirements }
ArrayTest >> collectionWithCopyNonIdentical [
	" return a collection that include elements for which 'copy' return a different object (this is not the case of SmallInteger)"
	^ collectionOfCollection
]

{ #category : #requirements }
ArrayTest >> collectionWithElementsToRemove [
	^ removedCollection
]

{ #category : #requirements }
ArrayTest >> collectionWithEqualElements [
	^ withEqualElements.
]

{ #category : #requirements }
ArrayTest >> collectionWithNonIdentitySameAtEndAndBegining [
	" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	^ stringCollectionWithSameBeginingAnEnd
]

{ #category : #requirements }
ArrayTest >> collectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	^ stringCollectionWithSameBeginingAnEnd
]

{ #category : #requirements }
ArrayTest >> collectionWithSortableElements [
" return a collection elements that can be sorte ( understanding message ' < '  or ' > ')"
	^ withoutEqualElements
]

{ #category : #requirements }
ArrayTest >> collectionWithoutEqualElements [
	^ withoutEqualElements
]

{ #category : #requirements }
ArrayTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	^ collectionWithoutNil
]

{ #category : #requirements }
ArrayTest >> elementInCollectionOfFloat [
	^ collectionOfFloat atRandom
]

{ #category : #requirements }
ArrayTest >> elementInForCopy [
	^ elementInForCopy
]

{ #category : #requirements }
ArrayTest >> elementInForElementAccessing [
" return an element inculded in 'accessCollection '"
	^ self accessCollection anyOne
]

{ #category : #requirements }
ArrayTest >> elementInForIncludesTest [

	^ elementInForCopy
]

{ #category : #requirements }
ArrayTest >> elementInForIndexAccess [
	^ elementInForCopy
]

{ #category : #requirements }
ArrayTest >> elementInForIndexAccessing [

	^ withoutEqualElements anyOne
]

{ #category : #requirements }
ArrayTest >> elementInForOccurrences [
	^ elementInForCopy
]

{ #category : #requirements }
ArrayTest >> elementInForReplacement [
	^ elementInForCopy
]

{ #category : #requirements }
ArrayTest >> elementNotIn [
"return an element not included in 'nonEmpty' "

	^ elementNotInForCopy
]

{ #category : #requirements }
ArrayTest >> elementNotInForCopy [
	^ elementNotInForCopy
]

{ #category : #requirements }
ArrayTest >> elementNotInForElementAccessing [
" return an element not included in 'accessCollection' "
	^ elementNotInForCopy
]

{ #category : #requirements }
ArrayTest >> elementNotInForIndexAccessing [

	^elementNotInForCopy
]

{ #category : #requirements }
ArrayTest >> elementNotInForOccurrences [
	^ elementNotInForCopy
]

{ #category : #'tests - copying' }
ArrayTest >> elementToAdd [
	^ 55
]

{ #category : #requirements }
ArrayTest >> elementTwiceInForOccurrences [
" return an element included exactly two time in # collectionWithEqualElements"
^ duplicateElement
]

{ #category : #requirements }
ArrayTest >> elementsCopyNonIdenticalWithoutEqualElements [
	" return a collection that does niot include equal elements ( classic equality )"
	^ collectionOfCollection
]

{ #category : #initialization }
ArrayTest >> empty [

	^ empty
]

{ #category : #'tests - iterate' }
ArrayTest >> expectedSizeAfterReject [
	^1
]

{ #category : #requirements }
ArrayTest >> firstCollection [
	^example1
]

{ #category : #requirements }
ArrayTest >> firstEven [
	"Returns the first even number of #collection"
	^ -2
]

{ #category : #requirements }
ArrayTest >> firstIndex [
	^ firstIndex
]

{ #category : #'tests - copying with replacement' }
ArrayTest >> firstIndexesOf: aSubCollection in: collection [
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

{ #category : #requirements }
ArrayTest >> firstOdd [
	"Returns the first odd number of #collection"
	^ 1
]

{ #category : #'tests - fixture' }
ArrayTest >> howMany: aSubCollection in: collection [
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
ArrayTest >> indexArray [
	^ indexArray .
]

{ #category : #requirements }
ArrayTest >> indexInForCollectionWithoutDuplicates [
	^ 2.
]

{ #category : #requirements }
ArrayTest >> indexInNonEmpty [
	"Return an index between bounds of 'nonEmpty'"
	
	^ 2
]

{ #category : #requirements }
ArrayTest >> integerCollection [
	^example1 .
]

{ #category : #requirements }
ArrayTest >> integerCollectionWithoutEqualElements [
	^{1. 2. 6. 5.}
]

{ #category : #requirements }
ArrayTest >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	^ example1
]

{ #category : #requirements }
ArrayTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	^ example1
]

{ #category : #requirements }
ArrayTest >> newElement [
	^999
]

{ #category : #initialization }
ArrayTest >> nonEmpty [

	^ example1
]

{ #category : #requirements }
ArrayTest >> nonEmpty1Element [

	^ nonEmpty1Element
]

{ #category : #requirements }
ArrayTest >> nonEmptyMoreThan1Element [
	^nonEmptyMoreThan1Element .
]

{ #category : #requirements }
ArrayTest >> oldSubCollection [
	^oldSubCollection
]

{ #category : #requirements }
ArrayTest >> replacementCollection [
	^replacementCollection .
]

{ #category : #requirements }
ArrayTest >> replacementCollectionSameSize [
	^replacementCollectionSameSize
]

{ #category : #initialization }
ArrayTest >> result [
	
	^ collectResult
]

{ #category : #requirements }
ArrayTest >> secondCollection [
	^example2
]

{ #category : #requirements }
ArrayTest >> secondIndex [
	^ secondIndex
]

{ #category : #parameters }
ArrayTest >> selectorToAccessValuePutIn [
	"return the selector of the method that should be invoked to access an element"
	
	^ #second
]

{ #category : #running }
ArrayTest >> setUp [

	super setUp.
	literalArray := #(1 true 3 #four).
	selfEvaluatingArray := { 1. true. (3/4). String loremIpsum. (2 to: 4) . 5 }.
	nonSEArray1 := { 1 . Set with: 1 }.
	nonSEarray2 := { testingEnvironment associationAt: #Array }.
	example1 := #(1 2 3 4 5) copy.
	indexArray:= {2. 3. 4.}.
	valueArray:={0. 0. 0.}.
	oldSubCollection:= {2. 3. 4.}.
	nonEmptyMoreThan1Element:= example1.
	subCollectionNotIn:= {1. 8. 3.}.
	collectionNotIncluded:= {7. 8. 9.}.
	removedCollection:=  { 2. 4. }.
	
	example2 := {1. 2. 3/4. 4. 5}.
	collectionWith4Elements := #(1 -2 3 1). 
	collectionWithoutNil := #( 1 2 3 4).
	simpleCollection := #( 1 8 3).
	collectionOfCollectionsOfInts := { 1. { 2. 3 }. { 4. { 5. 6} } }.
	collectionOfCollectionsOfStrings:= {{{'foo'}. {'bar'}}. 'zorg'}.
	collectResult := {SmallInteger. SmallInteger. SmallInteger. SmallInteger.}.
	empty := #().
	duplicateElement := 5.2.
	withEqualElements := {1.5. duplicateElement . 6.1. 2.0. duplicateElement .} .
	withoutEqualElements := {1.1. 4.4. 6.5. 2.4. 3.1.}.
	withCharacters := {$a. $x. $d. $c. $m.}.
	unsortedCollection := {1. 2.  8. 5. 6. 7.}.
	sortedInAscendingOrderCollection := {1. 2. 3. 4. 5. 6.}.
	elementInForCopy:= 2.
	elementNotInForCopy:= 9.
	firstIndex:= 2.
	secondIndex:= 4.
	replacementCollection:= {4. 3. 2. 1.}.
	replacementCollectionSameSize := {5. 4. 3.}.	
	nonEmpty1Element:={ 5.}.
	collectionOfCollection:={'a'. 'b'. 'c'.}.
	collectionOfFloat:={1.5. 5.5. 6.5.}.
	collectionOfFloatWithEqualElements:={1.5. 5.5. 6.5. 1.5}.
	stringCollectionWithSameBeginingAnEnd := {'c'. 's'. 'c' copy}.
	collection5Elements := { 1. 2. 5. 3. 4.}.
]

{ #category : #requirements }
ArrayTest >> simpleCollection [
	^simpleCollection.
]

{ #category : #requirements }
ArrayTest >> sizeCollection [

	^ self collection
]

{ #category : #requirements }
ArrayTest >> smallerIndex [
	^ firstIndex -1
]

{ #category : #requirements }
ArrayTest >> sortedInAscendingOrderCollection [
	^sortedInAscendingOrderCollection .
]

{ #category : #'tests - iterate' }
ArrayTest >> speciesClass [
	
	^ Array
]

{ #category : #requirements }
ArrayTest >> subCollectionNotIn [

	^subCollectionNotIn
]

{ #category : #'tests - fixture' }
ArrayTest >> test0FixtureIncludeTest [
	| anElementIn |
	self nonEmpty.
	self denyEmpty: self nonEmpty.
	self elementNotIn.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self elementNotIn ] ifNone: [ anElementIn := false ].
	self assert: anElementIn equals: false.
	self anotherElementNotIn.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self anotherElementNotIn ] ifNone: [ anElementIn := false ].
	self deny: anElementIn.
	self collection.
	self empty.
	self assertEmpty: self empty.
	self collectionOfFloat.
	self collectionOfFloat do: [ :each | self assert: each isFloat ].
	self elementInForIncludesTest.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self elementInForIncludesTest ] ifNone: [ anElementIn := false ].
	self assert: anElementIn
]

{ #category : #'tests - fixture' }
ArrayTest >> test0FixtureOccurrencesTest [
	self empty.
	self assertEmpty: self empty.
	self nonEmpty.
	self denyEmpty: self nonEmpty.
	self elementInForOccurrences.
	self assert: (self nonEmpty includes: self elementInForOccurrences).
	self elementNotInForOccurrences.
	self deny: (self nonEmpty includes: self elementNotInForOccurrences)
]

{ #category : #'tests - fixture' }
ArrayTest >> test0FixtureTConvertAsSetForMultiplinessTest [
	"a collection ofFloat with equal elements:"

	| res |
	self withEqualElements.
	self withEqualElements do: [ :each | self assert: each isFloat ].
	res := true.
	self withEqualElements detect: [ :each | (self withEqualElements occurrencesOf: each) > 1 ] ifNone: [ res := false ].
	self assert: res.	"a collection of Float without equal elements:"
	res := true.
	self elementsCopyNonIdenticalWithoutEqualElements
		detect: [ :each | (self elementsCopyNonIdenticalWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self deny: res
]

{ #category : #'tests - iterate' }
ArrayTest >> testAnySatisfy [

	self assert: ( self collection anySatisfy: [:each | each = -2]).
	self deny: (self collection anySatisfy: [:each | each isString]).
]

{ #category : #'tests - converting' }
ArrayTest >> testAsArrayKeepsIdentity [
	| x y |
	x := #(1 2 3 4 5).
	y := x asArray.
	self assert: y identityHash equals: x identityHash
]

{ #category : #'tests - as string comma delimiter sequenceable' }
ArrayTest >> testAsCommaStringEmpty [

	self assert: self empty asCommaString = ''.
	self assert: self empty asCommaStringAnd = ''
]

{ #category : #'tests - comma and delimiter' }
ArrayTest >> testAsCommaStringOne [
	
	"self assert: self oneItemCol asCommaString = '1'.
	self assert: self oneItemCol asCommaStringAnd = '1'."

	self assert: self nonEmpty1Element asCommaString = (self nonEmpty1Element first asString).
	self assert: self nonEmpty1Element asCommaStringAnd = (self nonEmpty1Element first asString)
]

{ #category : #'tests - converting' }
ArrayTest >> testAsIdentitySet [
	"Test with a collection without equal elements :"

	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: IdentitySet
]

{ #category : #'tests - converting' }
ArrayTest >> testAsSet [
 	"Test with a collection without equal elements"
	
	self assertSameContents: self collectionWithoutEqualElements whenConvertedTo: Set
]

{ #category : #'tests - as sorted collection' }
ArrayTest >> testAsSortedCollectionWithSortBlock [

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
ArrayTest >> testAsStringOnDelimiterLastMore [

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
ArrayTest >> testAsStringOnDelimiterLastOne [

	| delim oneItemStream result |	
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]

{ #category : #'tests - comma and delimiter' }
ArrayTest >> testAsStringOnDelimiterOne [

	| delim oneItemStream result |
	delim := ', '.
	result := ''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]

{ #category : #'tests - puting with indexes' }
ArrayTest >> testAtAllIndexesPut [
	
	self nonEmpty atAllPut: self aValue.
	self nonEmpty do:[ :each| self assert: each = self aValue].
]

{ #category : #'tests - puting with indexes' }
ArrayTest >> testAtAllPut [
 
	self nonEmpty atAll: self indexArray put: self aValue.
	
	self indexArray do:
		[:i | self assert: (self nonEmpty at: i) equals: self aValue ]
]

{ #category : #'tests - at put' }
ArrayTest >> testAtPut [
	
	self nonEmpty at: self anIndex put: self aValue.
	self assert: (self nonEmpty at: self anIndex) = self aValue.
]

{ #category : #'tests - at put' }
ArrayTest >> testAtPutOutOfBounds [
	
	self should: [self empty at: self anIndex put: self aValue] raise: Error
]

{ #category : #'tests - accessing' }
ArrayTest >> testAtWrap2 [
	| tabTest |
	tabTest := #(5 6 8).
	self assert: (tabTest atWrap: 2) equals: 6.
	self assert: (tabTest atWrap: 7) equals: 5.
	self assert: (tabTest atWrap: 5) equals: 6.
	self assert: (tabTest atWrap: 0) equals: 8.
	self assert: (tabTest atWrap: 1) equals: 5.
	self assert: (tabTest atWrap: -2) equals: 5
]

{ #category : #tests }
ArrayTest >> testBeReadOnlyLiteral [
	self assert: Array new beReadOnlyLiteral isReadOnlyObject.
	self assert: (Array with: String new) beReadOnlyLiteral first isReadOnlyObject.
	self deny: (Array with: Object new) first isReadOnlyObject
]

{ #category : #tests }
ArrayTest >> testCombinations [
	self assert: #(1 2 3) combinations equals: #(#(1) #(2) #(3) #(1 2) #(1 3) #(2 3) #(1 2 3)).
	self assert: (1 to: 3) combinations equals: #(#(1) #(2) #(3) #(1 2) #(1 3) #(2 3) #(1 2 3))
]

{ #category : #tests }
ArrayTest >> testCombinationsTaken [

	self
		assertCollection: (#( 1 2 3 ) combinations: 2)
		hasSameElements: #( #( 1 2 ) #( 1 3 ) #( 2 3 ) ).
	self
		assertCollection: ((1 to: 4) combinations: 3)
		hasSameElements: #( #( 1 2 3 ) #( 1 2 4 ) #( 1 3 4 ) #( 2 3 4 ) )
]

{ #category : #tests }
ArrayTest >> testComplexIsSelfEvaluating [
	| complexArray restoredArray |
	complexArray := {1 . true . false . nil . #a . 'a' . $a . Float pi . Float halfPi . (4 / 5) . Float infinity negated . (1 @ 2) . (0 @ 0 extent: 1 @ 1).
	('hola' -> 0) . Object . Object class}.
	complexArray := complexArray copyWith: complexArray.
	self assert: complexArray isSelfEvaluating.
	restoredArray := self class evaluate: complexArray printString.
	self assert: restoredArray equals: complexArray
]

{ #category : #'tests - copy - clone' }
ArrayTest >> testCopyCreatesNewObject [
	 
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: self nonEmpty == copy.
]

{ #category : #'tests - copy' }
ArrayTest >> testCopyNonEmptyWithout [
	
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

{ #category : #'tests - copy' }
ArrayTest >> testCopyNonEmptyWithoutAllNotIncluded [
]

{ #category : #'tests - copying with replacement' }
ArrayTest >> testCopyReplaceAllWith1Occurrence [
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
ArrayTest >> testCopyReplaceFromToWith [
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
ArrayTest >> testCopyReplaceFromToWithInsertion [
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
ArrayTest >> testCopyUpTo [

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
ArrayTest >> testCopyUpToLastWithDuplicate [

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
ArrayTest >> testDetectSequenced [
	"Testing that detect keep the first element returning true for sequenceable collections"

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element  detect: [:each | each notNil ].
	self assert: result equals: element
]

{ #category : #'tests - enumerating' }
ArrayTest >> testDifferencePreservesOrder [
	| n even odd odd2 |
	n := #(5 3 4).
	even := #(4).
	odd := n difference: even.
	odd2 := n reject: [ :i | i even ].
	self assert: odd equals: odd2
]

{ #category : #'tests - iterate' }
ArrayTest >> testDo [
	| res |
	res := OrderedCollection new.
	self collection do: [ :each | res add: each class ].
	self assert: res asArray equals: self result
]

{ #category : #'tests - iterate' }
ArrayTest >> testDo2 [
	| res |
	res := OrderedCollection new.
	self collection do: [ :each | res add: each class ].
	self assert: res asArray equals: self result
]

{ #category : #'tests - equality' }
ArrayTest >> testEqualSignIsTrueForEmptyButNonIdenticalCollections [
	
	self assert: self empty equals: self empty copy. 
	self assert: self empty copy equals: self empty.
	self assert: self empty copy equals: self empty copy
]

{ #category : #'tests - equality' }
ArrayTest >> testEqualSignOfIdenticalCollectionObjects [
	
	self assert: self empty equals: self empty. 
	self assert: self nonEmpty equals: self nonEmpty
]

{ #category : #'tests - find binary' }
ArrayTest >> testFindBinary [
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 1 - arg ]) equals: 1.
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 23 - arg ]) equals: 23.
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ]) equals: 11.
	self should: [ #(1 3 5 7 11 15 23) findBinary: [ :arg | 8 - arg ] ] raise: NotFound
]

{ #category : #'tests - find binary' }
ArrayTest >> testFindBinaryIfNone [
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ] ifNone: [ #none ]) equals: 11.
	self should: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 8 - arg ] ifNone: [ #none ]) = #none
]

{ #category : #'tests - iterate on sequenced reable collections' }
ArrayTest >> testFindFirst [

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element findFirst: [:each | each = element].
	
	self assert: result equals: 1
]

{ #category : #'tests - equality' }
ArrayTest >> testHasEqualElementsIsTrueForNonIdenticalButEqualCollections [
		
	self assert: (self empty hasEqualElements: self empty copy). 
	self assert: (self empty copy hasEqualElements: self empty).
	self assert: (self empty copy hasEqualElements: self empty copy).
		
	self assert: (self nonEmpty hasEqualElements: self nonEmpty copy). 
	self assert: (self nonEmpty copy hasEqualElements: self nonEmpty).
	self assert: (self nonEmpty copy hasEqualElements: self nonEmpty copy)
]

{ #category : #'tests - equality' }
ArrayTest >> testHasEqualElementsOfIdenticalCollectionObjects [
	
	self assert: (self empty hasEqualElements: self empty). 
	self assert: (self nonEmpty hasEqualElements: self nonEmpty)
]

{ #category : #'tests - includes' }
ArrayTest >> testIdentityIncludes [
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "

	" can not be used by collections that can't include elements for wich copy doesn't return another instance "

	| collection element |
	self collectionWithCopyNonIdentical.
	collection := self collectionWithCopyNonIdentical.
	element := collection anyOne copy.	"self assert: (collection includes: element)."
	self deny: (collection identityIncludes: element)
]

{ #category : #'tests - includes' }
ArrayTest >> testIdentityIncludesNonSpecificComportement [
	"Test the same comportement than #includes:"
	
	| collection |	
	collection := self nonEmpty.
	
	self deny: (collection identityIncludes: self elementNotIn).
	self assert: (collection identityIncludes: collection anyOne)
]

{ #category : #'tests - index access' }
ArrayTest >> testIdentityIndexOf [
	| collection element |
	element := self collectionWithCopyNonIdentical anyOne copy.
	self deny: self collectionWithCopyNonIdentical anyOne identicalTo: element.
	collection := self collectionWithCopyNonIdentical copyWith: element.
	self assert: (collection identityIndexOf: element) equals: collection size
]

{ #category : #'tests - set arithmetic' }
ArrayTest >> testIntersectionItself [
	
	| result |
	result := self collectionWithoutEqualElements intersection: self collectionWithoutEqualElements.
	self assert: result size equals: self collectionWithoutEqualElements size.
	self collectionWithoutEqualElements do: [ :each|
		self assert: (result includes: each) ].
]

{ #category : #tests }
ArrayTest >> testIsArray [
	
	self assert: example1 isArray
]

{ #category : #tests }
ArrayTest >> testIsLiteral [
	"We work with a copy of literalArray, to avoid corrupting the code."
	
	| aLiteralArray |
	aLiteralArray := literalArray copy.
	self assert: aLiteralArray isLiteral.
	aLiteralArray at: 1 put: self class.
	self deny: aLiteralArray isLiteral.
	self deny: (literalArray as: WeakArray) isLiteral description: 'instances of Array subclasses are not literal'.
]

{ #category : #tests }
ArrayTest >> testIsSelfEvaluating [

	self assert: example1 isSelfEvaluating.
	example1 at: 1 put: Bag new.
	self deny: example1 isSelfEvaluating.
	example1 at: 1 put: 1.
]

{ #category : #'tests - sorting' }
ArrayTest >> testIsSortedBy [
	self assert: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a<b]).
	self deny: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a>b]).
]

{ #category : #'tests - iterate on sequenced reable collections' }
ArrayTest >> testKeysAndValuesDo [
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

{ #category : #tests }
ArrayTest >> testLiteralEqual [

	self deny: (example1 literalEqual: example1 asIntegerArray)
]

{ #category : #tests }
ArrayTest >> testNewWithSize [
	| array |
	array := Array new: 5.
	self assert: array size equals: 5.
	1 to: 5 do: [ :index | self assert: (array at: index) isNil ]
]

{ #category : #'tests - occurrencesOf' }
ArrayTest >> testOccurrencesOf [
	| result expected |
	result := self nonEmpty occurrencesOf: self elementInForOccurrences.
	expected := 0.
	self nonEmpty do: [ :each | self elementInForOccurrences = each ifTrue: [ expected := expected + 1 ] ].
	self assert: result equals: expected
]

{ #category : #'tests - occurrencesOf' }
ArrayTest >> testOccurrencesOfEmpty [
	| result |
	result := self empty occurrencesOf: self elementInForOccurrences.
	self assert: result equals: 0
]

{ #category : #'tests - occurrencesOf for multipliness' }
ArrayTest >> testOccurrencesOfForMultipliness [

	| collection elem |
	collection := self collectionWithEqualElements.
	elem := self elementTwiceInForOccurrences.

	self assert: (collection occurrencesOf: elem ) equals: 2.
]

{ #category : #'tests - occurrencesOf' }
ArrayTest >> testOccurrencesOfNotIn [

	| result |
	result := self empty occurrencesOf: self elementNotInForOccurrences.
	self assert: result equals: 0
]

{ #category : #'tests - creation' }
ArrayTest >> testOfSize [
	
	| aCol |
	aCol := self collectionClass ofSize: 3.
	self assert: aCol size equals: 3.
]

{ #category : #'tests - arithmetic' }
ArrayTest >> testPremultiply [
	self assert: example1 +* #(2) equals: #(2 4 6 8 10)
]

{ #category : #tests }
ArrayTest >> testPrinting [
	self assert: literalArray printString equals: '#(1 true 3 #four)'.
	self assert: literalArray equals: (self class compiler evaluate: literalArray printString).
	self assert: selfEvaluatingArray equals: (self class compiler evaluate: selfEvaluatingArray printString).
	self assert: nonSEArray1 printString equals: 'an Array(1 a Set(1))'.
	self assert: nonSEarray2 printString equals: '{#Array->Array}'
]

{ #category : #'tests - iterate on sequenced reable collections' }
ArrayTest >> testReverseWithDo [
	
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

{ #category : #'tests - shuffling' }
ArrayTest >> testShuffleBy [

	| base random shuffleByResult |
	base := #(1 2 3 4 5) copy.
	random := (Random seed: 42).
	shuffleByResult := #(2 5 4 3 1) copy.
	
	self assert: ((base shuffleBy: random) hasEqualElements: shuffleByResult).
]

{ #category : #'tests - shuffling' }
ArrayTest >> testShuffleChangeOrder [

	| array shuffled |
	array := #( 1 2 3 4 5).
	shuffled := array copy shuffle.
	array do: [ :e | self assert: (shuffled includes: e) ]
]

{ #category : #'tests - shuffling' }
ArrayTest >> testShuffleModifyTheReceiver [

	| array shuffled |
	array := #( 1 2 3 4 5) copy.
	shuffled := array shuffle.
	self assert: array == shuffled.
]

{ #category : #'tests - arithmetic' }
ArrayTest >> testSumNumberItemsWithoutBlock [
	
	self assert: #() sumNumbers equals: 0.
	
	self assert: #(1 2 3) sumNumbers equals: 6
]

{ #category : #'tests - puting with indexes' }
ArrayTest >> testSwapWith [
 
	| result index |
	index := self indexArray anyOne.
	result:= self nonEmpty copy .
	result swap: index with: 1.
	self assert: (result at: index) = (self nonEmpty at:1).
	self assert: (result at: 1) = (self nonEmpty at: index).
]

{ #category : #'tests - iterate on sequenced reable collections' }
ArrayTest >> testWithDo [
	
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
ArrayTest >> testWithIndexCollect [
	
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
ArrayTest >> testWithIndexDo [

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
ArrayTest >> testWithWith [
	
	| aCol collection element1 element2 |
	collection := self collectionMoreThan5Elements asOrderedCollection copyFrom: 1 to: 2.
	element1 := collection at: 1.
	element2 := collection at: 2.
	
	aCol := self collectionClass with: element1  with: element2 .
	self assert: (aCol occurrencesOf: element1 ) equals: (collection occurrencesOf: element1).
	self assert: (aCol occurrencesOf: element2 ) equals: (collection occurrencesOf: element2)
]

{ #category : #'tests - begins ends with' }
ArrayTest >> testsBeginsWithEmpty [
	
	self deny: (self nonEmpty beginsWith:(self empty)).
	self deny: (self empty beginsWith:(self nonEmpty))
]

{ #category : #'tests - begins ends with' }
ArrayTest >> testsEndsWithEmpty [
	
	self deny: (self nonEmpty endsWith: self empty).
	self deny: (self empty endsWith: self nonEmpty)
]

{ #category : #requirements }
ArrayTest >> unsortedCollection [
	^unsortedCollection .
]

{ #category : #parameters }
ArrayTest >> valuePutIn [
	"the value that we will put in the non empty collection"
	
	^ 2
]

{ #category : #requirements }
ArrayTest >> withEqualElements [
	" return a collection of float including equal elements (classic equality)"
	^ collectionOfFloatWithEqualElements
]
