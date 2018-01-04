"
This is the unit test for the class Array. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org/)	
	- the sunit class category
"
Class {
	#name : #ArrayTest,
	#superclass : #CollectionRootTest,
	#traits : 'TEmptySequenceableTest + TSequencedElementAccessTest + TCloneTest + TIncludesWithIdentityCheckTest + TCopyTest + TCreationWithTest + TPutBasicTest + (TConvertTest - {}) + TOccurrencesForMultiplinessTest + TIterateSequencedReadableTest + TSequencedConcatenationTest + TReplacementSequencedTest + TAsStringCommaAndDelimiterSequenceableTest + TBeginsEndsWith + TPrintOnSequencedTest + TIndexAccess + TSubCollectionAccess + TConvertAsSetForMultiplinessIdentityTest + TCopyPartOfSequenceable + TCopySequenceableSameContents + TCopySequenceableWithOrWithoutSpecificElements + TCopySequenceableWithReplacement + TIndexAccessForMultipliness + TCopyPartOfSequenceableForMultipliness + TConvertAsSortedTest + TPutTest + TSequencedStructuralEqualityTest + TSortTest + TSetArithmetic + TEnumeratingTest',
	#classTraits : 'TEmptySequenceableTest classTrait + TSequencedElementAccessTest classTrait + TCloneTest classTrait + TIncludesWithIdentityCheckTest classTrait + TCopyTest classTrait + TCreationWithTest classTrait + TPutBasicTest classTrait + TConvertTest classTrait + TOccurrencesForMultiplinessTest classTrait + TIterateSequencedReadableTest classTrait + TSequencedConcatenationTest classTrait + TReplacementSequencedTest classTrait + TAsStringCommaAndDelimiterSequenceableTest classTrait + TBeginsEndsWith classTrait + TPrintOnSequencedTest classTrait + TIndexAccess classTrait + TSubCollectionAccess classTrait + TConvertAsSetForMultiplinessIdentityTest classTrait + TCopyPartOfSequenceable classTrait + TCopySequenceableSameContents classTrait + TCopySequenceableWithOrWithoutSpecificElements classTrait + TCopySequenceableWithReplacement classTrait + TIndexAccessForMultipliness classTrait + TCopyPartOfSequenceableForMultipliness classTrait + TConvertAsSortedTest classTrait + TPutTest classTrait + TSequencedStructuralEqualityTest classTrait + TSortTest classTrait + TSetArithmetic classTrait + TEnumeratingTest classTrait',
	#instVars : [
		'example1',
		'literalArray',
		'selfEvaluatingArray',
		'otherArray',
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
		'sizeCollection',
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
		'floatCollectionWithSameBeginingAnEnd',
		'collectionWithoutNil',
		'duplicateElement',
		'collection5Elements',
		'collectionWith4Elements',
		'collectionOfCollectionsOfStrings',
		'collectionOfCollectionsOfInts',
		'simpleCollection'
	],
	#category : #'Collections-Tests-Arrayed'
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

{ #category : #'tests - converting' }
ArrayTest >> assertSameContents: aCollection whenConvertedTo: aClass [ 
	| result |
	result := aCollection perform: ('as' , aClass name) asSymbol.
	self assert: (result class includesBehavior: aClass).
	result do: 
		[ :each | 
		self assert: (aCollection occurrencesOf: each) = (result occurrencesOf: each) ].
	self assert: result size = aCollection size
]

{ #category : #initialization }
ArrayTest >> collection [ 

	^ collectionWith4Elements 
]

{ #category : #'tests - creation' }
ArrayTest >> collectionClass [ 
	
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
	^ collectionOfCollection
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
	^ floatCollectionWithSameBeginingAnEnd 
]

{ #category : #requirements }
ArrayTest >> collectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	^ floatCollectionWithSameBeginingAnEnd 
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
	^ collectionOfCollection atRandom
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

{ #category : #requirements }
ArrayTest >> firstOdd [
	"Returns the first odd number of #collection"
	^ 1
]

{ #category : #requirements }
ArrayTest >> floatCollectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	^ floatCollectionWithSameBeginingAnEnd 
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
	nonSEarray2 := { Smalltalk globals associationAt: #Array }.
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
	collectionOfCollection:={1.5. 5.5. 6.5.}.
	collectionOfFloatWithEqualElements:={1.5. 5.5. 6.5. 1.5}.
	floatCollectionWithSameBeginingAnEnd := {1.5. 5.5. 1.5 copy}.
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
ArrayTest >> test0CopyTest [
	self empty.
	self assert: self empty size = 0.
	self nonEmpty.
	self assert: (self nonEmpty size = 0) not.
	self collectionWithElementsToRemove.
	self assert: (self collectionWithElementsToRemove size = 0) not.
	self elementToAdd
]

{ #category : #'tests - fixture' }
ArrayTest >> test0FixtureIncludeTest [
	| anElementIn |
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self elementNotIn.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self elementNotIn ] ifNone: [ anElementIn := false ].
	self assert: anElementIn = false.
	self anotherElementNotIn.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self anotherElementNotIn ] ifNone: [ anElementIn := false ].
	self assert: anElementIn = false.
	self collection.
	self empty.
	self assert: self empty isEmpty.
	self collectionOfFloat.
	self collectionOfFloat do: [ :each | self assert: each isFloat ].
	self elementInForIncludesTest.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self elementInForIncludesTest ] ifNone: [ anElementIn := false ].
	self assert: anElementIn = true
]

{ #category : #'tests - fixture' }
ArrayTest >> test0FixtureOccurrencesTest [
	self empty.
	self assert: self empty isEmpty.
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
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
	self assert: res = true.	"a collection of Float without equal elements:"
	self elementsCopyNonIdenticalWithoutEqualElements.
	self elementsCopyNonIdenticalWithoutEqualElements do: [ :each | self assert: each isFloat ].
	res := true.
	self elementsCopyNonIdenticalWithoutEqualElements
		detect: [ :each | (self elementsCopyNonIdenticalWithoutEqualElements occurrencesOf: each) > 1 ]
		ifNone: [ res := false ].
	self assert: res = false
]

{ #category : #'tests - iterate' }
ArrayTest >> testAnySatisfy [

	self assert: ( self collection anySatisfy: [:each | each = -2]).
	self deny: (self collection anySatisfy: [:each | each isString]).
]

{ #category : #'tests - converting' }
ArrayTest >> testAsArrayKeepsIdentity [
	 
	|x y|
	x := #(1 2 3 4 5).
	y := x asArray.
	self assert: y identityHash = x identityHash
]

{ #category : #'tests - accessing' }
ArrayTest >> testAtWrap2 [
	| tabTest |
	tabTest := #(5 6 8 ).
	self assert: (tabTest atWrap: 2) = 6.
	self assert: (tabTest atWrap: 7) = 5.
	self assert: (tabTest atWrap: 5) = 6.
	self assert: (tabTest atWrap: 0) = 8.
	self assert: (tabTest atWrap: 1) = 5.
	self assert: (tabTest atWrap: -2) = 5
]

{ #category : #tests }
ArrayTest >> testCombinations [

	self assert: #(1 2 3) combinations = #(#(1) #(2) #(3) #(1 2) #(1 3) #(2 3) #(1 2 3)).
	self assert: (1 to: 3) combinations = #(#(1) #(2) #(3) #(1 2) #(1 3) #(2 3) #(1 2 3)).
]

{ #category : #tests }
ArrayTest >> testComplexIsSelfEvaluating [
	"self run: #testComplexIsSelfEvaluating"

	| complexArray restoredArray |
	complexArray := {1.
	true.
	false.
	nil.
	#a.
	'a'.
	$a.
	(Float pi).
	(Float halfPi).
	(4 / 5).
	(Float infinity negated).
	(1 @ 2).
	(0 @ 0 extent: 1 @ 1).
	('hola' -> 0).
	Object.
	(Object class)}.
	complexArray := complexArray copyWith: complexArray.
	self assert: complexArray isSelfEvaluating.
	restoredArray := self class evaluate: complexArray printString.
	self assert: restoredArray = complexArray
]

{ #category : #'tests - copy' }
ArrayTest >> testCopyNonEmptyWithoutAllNotIncluded [
	
]

{ #category : #'tests - iterate' }
ArrayTest >> testDo [

	| res |
	res := OrderedCollection new.  
	self collection do: [:each | res add: each class].
	self assert: res asArray = self result.
]

{ #category : #'tests - iterate' }
ArrayTest >> testDo2 [

	| res |
	res := OrderedCollection new.  
	self collection do: [:each | res add: each class].
	self assert: res asArray = self result. 
]

{ #category : #'tests - find binary' }
ArrayTest >> testFindBinary [
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 1 - arg ]) = 1.
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 23 - arg ]) = 23.
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ]) = 11.
	self should: [ #(1 3 5 7 11 15 23) findBinary: [ :arg | 8 - arg ] ] raise: NotFound
]

{ #category : #'tests - find binary' }
ArrayTest >> testFindBinaryIfNone [
	self assert: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 11 - arg ] ifNone: [ #none ]) = 11.
	self should: (#(1 3 5 7 11 15 23) findBinary: [ :arg | 8 - arg ] ifNone: [ #none ]) = #none
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

{ #category : #'tests - index access' }
ArrayTest >> testIdentityIndexOf [
	"self debug: #testIdentityIndexOf"
	| collection element |
	element := self elementInCollectionOfFloat copy.
	self deny: self elementInCollectionOfFloat == element.
	collection := self collectionOfFloat copyWith: element.
	self assert: (collection identityIndexOf: element) = collection size
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

{ #category : #tests }
ArrayTest >> testLiteralEqual [

	self deny: (example1 literalEqual: example1 asIntegerArray)
]

{ #category : #tests }
ArrayTest >> testNewWithSize [
	|array|
	array := Array new: 5.
	self assert: array size = 5.
	1 to: 5 do: [:index | self assert: (array at: index) isNil]
]

{ #category : #'tests - occurrencesOf' }
ArrayTest >> testOccurrencesOf [
	| result expected |
	result := self nonEmpty occurrencesOf: self elementInForOccurrences.
	expected := 0.
	self nonEmpty do: [ :each | self elementInForOccurrences = each ifTrue: [ expected := expected + 1 ] ].
	self assert: result = expected
]

{ #category : #'tests - occurrencesOf' }
ArrayTest >> testOccurrencesOfEmpty [
	| result |
	result := self empty occurrencesOf: self elementInForOccurrences.
	self assert: result = 0
]

{ #category : #'tests - occurrencesOf' }
ArrayTest >> testOccurrencesOfNotIn [
	| result |
	result := self empty occurrencesOf: self elementNotInForOccurrences.
	self assert: result = 0
]

{ #category : #'tests - arithmetic' }
ArrayTest >> testPremultiply [
	self assert: example1 +* #(2 ) = #(2 4 6 8 10 ) 
]

{ #category : #tests }
ArrayTest >> testPrinting [
	"self debug: #testPrinting"
	
	self assert: literalArray printString = '#(1 true 3 #four)'.
	self assert: (literalArray = (self class compiler evaluate: literalArray printString)).
	self assert: (selfEvaluatingArray = (self class compiler evaluate: selfEvaluatingArray printString)).
	self assert: nonSEArray1 printString =  'an Array(1 a Set(1))'.
	self assert: nonSEarray2 printString = '{#Array->Array}'

]

{ #category : #'tests - arithmetic' }
ArrayTest >> testSumNumberItemsWithoutBlock [
	
	self assert: #() sumNumbers equals: 0.
	
	self assert: #(1 2 3) sumNumbers equals: 6
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
