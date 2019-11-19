"
SUnit tests for intervals
"
Class {
	#name : #IntervalTest,
	#superclass : #CollectionRootTest,
	#traits : 'TSortTest - {#testSort. #testSortUsingSortBlock} + TCloneTest + TIncludesWithIdentityCheckTest + TSequencedElementAccessTest + TIterateSequencedReadableTest + TSequencedConcatenationTest + TSubCollectionAccess + TAsStringCommaAndDelimiterSequenceableTest + TIndexAccess + TPrintOnSequencedTest + TConvertTest + (TCopySequenceableWithReplacement - {#testCopyReplaceAllWithManyOccurence. #collectionWith2TimeSubcollection}) + TCopySequenceableWithOrWithoutSpecificElements + (TCopySequenceableSameContents - {#testShuffled}) + (TCopyPartOfSequenceable - {#testCopyEmptyMethod}) + TCopyTest + TBeginsEndsWith + TConvertAsSortedTest + TSequencedStructuralEqualityTest + TOccurrencesTest',
	#classTraits : 'TSortTest classTrait + TCloneTest classTrait + TIncludesWithIdentityCheckTest classTrait + TSequencedElementAccessTest classTrait + TIterateSequencedReadableTest classTrait + TSequencedConcatenationTest classTrait + TSubCollectionAccess classTrait + TAsStringCommaAndDelimiterSequenceableTest classTrait + TIndexAccess classTrait + TPrintOnSequencedTest classTrait + TConvertTest classTrait + TCopySequenceableWithReplacement classTrait + TCopySequenceableWithOrWithoutSpecificElements classTrait + TCopySequenceableSameContents classTrait + TCopyPartOfSequenceable classTrait + TCopyTest classTrait + TBeginsEndsWith classTrait + TConvertAsSortedTest classTrait + TSequencedStructuralEqualityTest classTrait + TOccurrencesTest classTrait',
	#instVars : [
		'empty',
		'nonEmpty',
		'one',
		'elementIn',
		'elementNotIn',
		'subCollectionNotIn',
		'collectionOfFloat',
		'anotherCollection',
		'nonEmpty1Element',
		'subCollection',
		'collectionWithSubCollection',
		'subCollectionInNonEmpty',
		'collectionWithoutNil',
		'collectResult',
		'collectionOfLargeIntegers'
	],
	#category : #'Collections-Tests-Sequenceable'
}

{ #category : #requirements }
IntervalTest >> accessCollection [
	^ -2 to: 14 by: 4
]

{ #category : #requirements }
IntervalTest >> anotherElementNotIn [
	^ 42
]

{ #category : #requirements }
IntervalTest >> collection [
	^ nonEmpty
]

{ #category : #requirements }
IntervalTest >> collectionClass [
	"Return the class to be used to create instances of the class tested"
	
	^ Interval
]

{ #category : #requirements }
IntervalTest >> collectionInForIncluding [
	^ nonEmpty copyWithout: (self nonEmpty last).
]

{ #category : #requirements }
IntervalTest >> collectionMoreThan1NoDuplicates [
	" return a collection of size 5 without equal elements"
	^ nonEmpty 
]

{ #category : #requirements }
IntervalTest >> collectionNotIncluded [
	^ (nonEmpty last + 1) to: (nonEmpty last +5)
]

{ #category : #requirements }
IntervalTest >> collectionOfFloat [
	^collectionOfFloat 
]

{ #category : #requirements }
IntervalTest >> collectionWith1TimeSubcollection [
	^ collectionWithSubCollection 
]

{ #category : #requirements }
IntervalTest >> collectionWithCopyNonIdentical [
	" return a collection that include elements for which 'copy' return a different object (this is not the case of SmallInteger)"
	^ collectionOfLargeIntegers
]

{ #category : #requirements }
IntervalTest >> collectionWithElementsToRemove [
	
	^ subCollectionInNonEmpty .
]

{ #category : #requirements }
IntervalTest >> collectionWithSortableElements [
" return a collection elements that can be sorte ( understanding message ' < '  or ' > ')"
	^ self nonEmpty 
]

{ #category : #requirements }
IntervalTest >> collectionWithoutEqualElements [
	^ nonEmpty 
]

{ #category : #requirements }
IntervalTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	^ collectionWithoutNil 
]

{ #category : #requirements }
IntervalTest >> doWithoutNumber [

	^ 6
]

{ #category : #requirements }
IntervalTest >> elementInCollectionOfFloat [
	^ collectionOfFloat anyOne
]

{ #category : #requirements }
IntervalTest >> elementInForElementAccessing [
" return an element inculded in 'accessCollection '"
	^ self accessCollection anyOne
]

{ #category : #requirements }
IntervalTest >> elementInForIncludesTest [

	^ elementIn 
]

{ #category : #requirements }
IntervalTest >> elementInForIndexAccess [
	^ self accessCollection  anyOne
]

{ #category : #requirements }
IntervalTest >> elementInForIndexAccessing [

	^ elementIn 
]

{ #category : #requirements }
IntervalTest >> elementNotIn [
	^elementNotIn
]

{ #category : #requirements }
IntervalTest >> elementNotInForElementAccessing [
" return an element not included in 'accessCollection' "
	^ elementNotIn 
]

{ #category : #requirements }
IntervalTest >> elementNotInForIndexAccessing [

	^elementNotIn 
]

{ #category : #requirements }
IntervalTest >> elementNotInForOccurrences [
	^ 9
]

{ #category : #requirements }
IntervalTest >> elementToAdd [
	^ elementNotIn 
]

{ #category : #requirements }
IntervalTest >> empty [

	^ empty

]

{ #category : #requirements }
IntervalTest >> expectedSizeAfterReject [
	"Number of even elements in #collection"
	^ 3
]

{ #category : #requirements }
IntervalTest >> firstCollection [
	^ nonEmpty.
]

{ #category : #requirements }
IntervalTest >> firstEven [
	"Returns the first even number of #collection"
	^ -2
]

{ #category : #requirements }
IntervalTest >> firstOdd [
	"Returns the first odd number of #collection"
	^ -5
]

{ #category : #requirements }
IntervalTest >> indexInForCollectionWithoutDuplicates [
	^ 2.
]

{ #category : #requirements }
IntervalTest >> indexInNonEmpty [
	"Return an index between bounds of 'nonEmpty'"
	
	^ 2
]

{ #category : #requirements }
IntervalTest >> integerCollection [
	^ nonEmpty 
]

{ #category : #requirements }
IntervalTest >> integerCollectionWithoutEqualElements [
	^ 1 to: 23.
]

{ #category : #requirements }
IntervalTest >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	^ nonEmpty 
]

{ #category : #requirements }
IntervalTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	^ nonEmpty 
]

{ #category : #requirements }
IntervalTest >> nonEmpty [

	^ nonEmpty
]

{ #category : #requirements }
IntervalTest >> nonEmpty1Element [

	^ nonEmpty1Element  
]

{ #category : #requirements }
IntervalTest >> nonEmptyMoreThan1Element [
	^nonEmpty .
]

{ #category : #requirements }
IntervalTest >> oldSubCollection [
	^ subCollection 
]

{ #category : #requirements }
IntervalTest >> replacementCollection [
	^ 5 to: 7.
]

{ #category : #requirements }
IntervalTest >> result [
	^ {SmallInteger . SmallInteger . SmallInteger . SmallInteger . SmallInteger . SmallInteger}
]

{ #category : #requirements }
IntervalTest >> secondCollection [
	^anotherCollection 
]

{ #category : #running }
IntervalTest >> setUp [
	super setUp.
	empty := (1 to: 0).
	one := (1 to:1).
	nonEmpty := -5 to: 10 by: 3.
	subCollectionInNonEmpty := -2 to: 4 by: 3.
	nonEmpty1Element:= 1to:1.
	anotherCollection:= 2 to: 15.
	collectionWithoutNil := 1 to: 3.
	collectResult := { SmallInteger. SmallInteger. SmallInteger.}.
	elementIn :=-2.
	elementNotIn:= 12.
	subCollectionNotIn:= -2 to: 1.
	collectionOfFloat := 1.5 to: 7.5 by: 1.
	subCollection := 2 to: 8.
	collectionWithSubCollection := 1 to: 10.
	collectionOfLargeIntegers := 100 factorial to: 100 factorial + 5.
]

{ #category : #requirements }
IntervalTest >> sizeCollection [
	"Answers a collection whose #size is 4"
	^ 1 to: 4.
]

{ #category : #requirements }
IntervalTest >> sortedInAscendingOrderCollection [
	"Return a collection sorted in ascending order"
	^ anotherCollection
]

{ #category : #requirements }
IntervalTest >> speciesClass [
	
	^ Array
]

{ #category : #requirements }
IntervalTest >> subCollectionNotIn [

	^subCollectionNotIn 
]

{ #category : #'tests - fixture' }
IntervalTest >> test0IndexAccessingTest [
	self accessCollection.
	self assert: self accessCollection size equals: 5.
	self subCollectionNotIn.
	self subCollectionNotIn detect: [ :each | (self accessCollection includes: each) not ] ifNone: [ self assert: false ].
	self elementNotInForIndexAccessing.
	self deny: (self accessCollection includes: self elementNotInForIndexAccessing).
	self elementInForIndexAccessing.
	self assert: (self accessCollection includes: self elementInForIndexAccessing).
	self collectionOfFloat.
	self collectionOfFloat do: [ :each | self deny: each class equals: SmallInteger ]
]

{ #category : #tests }
IntervalTest >> testAdd [
	self assert: (1 to: 10) + 5 equals: (6 to: 15)
]

{ #category : #'tests - indexable access' }
IntervalTest >> testAllButFirstElements [
	| abf col |
	col := self accessCollection.
	abf := col allButFirst: 2.
	1 to: abf size do: [ :i | self assert: (abf at: i) equals: (col at: i + 2) ].
	self assert: abf size + 2 equals: col size
]

{ #category : #'tests - indexable access' }
IntervalTest >> testAllButLastElements [
	| abf col |
	col := self accessCollection.
	abf := col allButLast: 2.
	1 to: abf size do: [ :i | self assert: (abf at: i) equals: (col at: i) ].
	self assert: abf size + 2 equals: col size
]

{ #category : #tests }
IntervalTest >> testAsInterval [
	"This is the same as newFrom:"

	self assert: (#(1 2 3) as: Interval) equals: (1 to: 3).
	self assert: (#(33 5 -23) as: Interval) equals: (33 to: -23 by: -28).
	self assert: (#[2 4 6] as: Interval) equals: (2 to: 6 by: 2).

	self should: [#(33 5 -22) as: Interval]
		raise: Error
		description: 'This is not an arithmetic progression'

]

{ #category : #tests }
IntervalTest >> testAt [
	self assert: ((1 to: 10) at: 1) equals: 1.
	self assert: ((1 to: 10) at: 3) equals: 3.
	self assert: ((1 to: 10 by: 2) at: 1) equals: 1.
	self assert: ((1 to: 10 by: 2) at: 3) equals: 5
]

{ #category : #'tests - indexable access' }
IntervalTest >> testAtWrap [
	"
	self assert: (self accessCollection at: 1) = 1.
	self assert: (self accessCollection at: 2) = 2.
	"

	| index |
	index := self accessCollection indexOf: self elementInForIndexAccessing.
	self assert: (self accessCollection atWrap: index) equals: self elementInForIndexAccessing.
	self assert: (self accessCollection atWrap: index + self accessCollection size) equals: self elementInForIndexAccessing.
	self assert: (self accessCollection atWrap: index - self accessCollection size) equals: self elementInForIndexAccessing.
	self assert: (self accessCollection atWrap: 1 + self accessCollection size) equals: (self accessCollection at: 1)
]

{ #category : #tests }
IntervalTest >> testCollectThenSelectLocal [
	| letters vowels |
	letters := 'abcdefghijklmnopqrstuvwxyz'.
	vowels := (1 to: 26) collect: [:index | letters at: index] thenSelect: [:char | char isVowel].
	self assert: (vowels hasEqualElements: 'aeiou').
]

{ #category : #tests }
IntervalTest >> testDo [
	| s i |
	s := OrderedCollection new.
	i := (10 to: 20).
	i do: [ :each | s addLast: each].
	self assert: (s hasEqualElements: i)
]

{ #category : #tests }
IntervalTest >> testEquals [
	self assert: (3 to: 5) equals: #(3 4 5).
	self deny: (3 to: 5) equals: #(3 5).
	self deny: (3 to: 5) equals: #().

	self assert: #(3 4 5) equals: (3 to: 5).
	self deny: #(3 5) equals: (3 to: 5).
	self deny: #() equals: (3 to: 5)
]

{ #category : #tests }
IntervalTest >> testEquals2 [
	self assert: (3 to: 5) equals: #(3 4 5).
	self deny: (3 to: 5) equals: #(3 5).
	self deny: (3 to: 5) equals: #().

	self assert: #(3 4 5) equals: (3 to: 5).
	self deny: #(3 5) equals: (3 to: 5).
	self deny: #() equals: (3 to: 5)
]

{ #category : #tests }
IntervalTest >> testEquals3 [
	self assert: (3 to: 5 by: 2) first equals: (3 to: 6 by: 2) first.
	self assert: (3 to: 5 by: 2) last equals: (3 to: 6 by: 2) last.
	self assert: (3 to: 5 by: 2) equals: (3 to: 6 by: 2)
]

{ #category : #tests }
IntervalTest >> testEquals4 [
	self assert: (3 to: 5 by: 2) equals: #(3 5).
	self deny: (3 to: 5 by: 2) equals: #(3 4 5).
	self deny: (3 to: 5 by: 2) equals: #().

	self assert: #(3 5) equals: (3 to: 5 by: 2).
	self deny: #(3 4 5) equals: (3 to: 5 by: 2).
	self deny: #() equals: (3 to: 5 by: 2)
]

{ #category : #tests }
IntervalTest >> testExtent [
	self assert: (1 to: 10) extent equals: 9.
	self assert: (1 to: 10 by: 2) extent equals: 9.
	self assert: (1 to: -1) extent equals: -2
]

{ #category : #'tests - includes' }
IntervalTest >> testIdentityIncludes [
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "

	" can not be used by collections that can't include elements for wich copy doesn't return another instance "

	| collection element |
	self collectionWithCopyNonIdentical.
	collection := self collectionWithCopyNonIdentical.
	element := collection anyOne copy.	"self assert: (collection includes: element)."
	self deny: (collection identityIncludes: element)
]

{ #category : #'tests - includes' }
IntervalTest >> testIncludesElementIsNotThere [
	
	self deny: (self nonEmpty includes: self elementNotInForOccurrences).
	self assert: (self nonEmpty includes: self nonEmpty anyOne).
	self deny: (self empty includes: self elementNotInForOccurrences)
]

{ #category : #tests }
IntervalTest >> testInclusion [
	"Non regression test for another bug of fuzzy inclusion"
	self deny: ((1.0 to: 3.0 by: 1.0 successor) includes: 3.0) description: 'The last element of this Interval is closed to 2'
]

{ #category : #tests }
IntervalTest >> testInclusionBug1603 [
	"This test is by german morales.
	It is about mantis bug 1603"
	
	self shouldnt: ((1 to: 5 by: 1) includes: 2.5). "obvious"
	self shouldnt: ((100000000000000 to: 500000000000000 by: 100000000000000)
 		  includes: 250000000000000). "same as above with 14 zeros appended"
]

{ #category : #tests }
IntervalTest >> testInclusionBug6455 [
	"This test is about mantis bug http://bugs.squeak.org/view.php?id=6455
	It should work as long as Fuzzy inclusion test feature for Interval of Float is maintained.
	This is a case when tested element is near ones of actual value, but by default.
	Code used to work only in the case of close numbers by excess..."
	
	self assert: ((0 to: Float pi by: Float pi / 100) includes: Float pi * (3/100))
]

{ #category : #tests }
IntervalTest >> testIndexOfBug1602 [
	"This test is by german morales.
	It is about mantis bug 1602"
	
	self should: ((1 to: 5 by: 1) indexOf: 2.5) = 0. "obvious"
	self should: ((100000000000000 to: 500000000000000 by: 100000000000000)
 		  indexOf: 250000000000000) = 0. "same as above with 14 zeros appended"
]

{ #category : #tests }
IntervalTest >> testIndexOfBug6455 [
	"This test is about mantis bug http://bugs.squeak.org/view.php?id=6455
	It should work as long as Fuzzy inclusion test feature for Interval of Float is maintained.
	This is a case when tested element is near ones of actual value, but by default.
	Code used to work only in the case of close numbers by excess..."

	self assert: ((0 to: Float pi by: Float pi / 100) indexOf: Float pi * (3 / 100)) equals: 4
]

{ #category : #tests }
IntervalTest >> testInfiniteLoopBug6456 [
	"This is a non regression test against mantis bug #6456.
	Some Float interval size was not consistent with do: loop.
	Some Float Interval used to do: infinite loops"

	| x interval counter size |
	x := (1.0 timesTwoPower: Float precision). "Note: x+1 = x due to inexact arithmetic"
	interval := x to: x+4.
	size := interval size.
	counter := 0.
	interval do: [:each | self assert: (counter := counter + 1) <= size]

]

{ #category : #tests }
IntervalTest >> testIntervalStoreOn [
	"this is a test for http://bugs.squeak.org/view.php?id=4378"

	| interval1 interval2 |
	interval1 := 0 to: 1 by: 0.5s1 squared.
	interval2 := self class compiler evaluate: interval1 storeString.
	self assert: interval1 size equals: interval2 size
]

{ #category : #tests }
IntervalTest >> testInvalid [
	"empty, impossible ranges"

	self assert: (1 to: 0) equals: #().
	self assert: (1 to: -1) equals: #().
	self assert: (-1 to: -2) equals: #().
	self assert: (1 to: 5 by: -1) equals: #().

	"always contains only start value."
	self assert: (1 to: 1) equals: #(1).
	self assert: (1 to: 5 by: 10) equals: #(1).
	self assert: (1 to: 0 by: -2) equals: #(1).

	"zero steps are not allowed"
	self should: [ 1 to: 1 by: 0 ] raise: DomainError
]

{ #category : #tests }
IntervalTest >> testIsEvaluating [
	self assert: (1 to: 10) isSelfEvaluating.
	self assert: (1 to: 10 by: 2) isSelfEvaluating
]

{ #category : #tests }
IntervalTest >> testIsInterval [
	self assert: (1 to: 10) isInterval.
	self assert: (1 to: 10 by: 2) isInterval
]

{ #category : #tests }
IntervalTest >> testLast [
	self assert: (1 to: 10) last equals: 10.
	self assert: (1 to: 10 by: 2) last equals: 9
]

{ #category : #tests }
IntervalTest >> testMinus [
	self assert: (1 to: 10) - 5 equals: (-4 to: 5)
]

{ #category : #tests }
IntervalTest >> testNewFrom [
	self assert: (Interval newFrom: (1 to: 1)) equals: (1 to: 1).
	self assert: (Interval newFrom: #(1)) equals: (1 to: 1).
	self assert: (Interval newFrom: #()) equals: (1 to: 0)
]

{ #category : #tests }
IntervalTest >> testNumericTypes [

	self assert: ((3 asNumber) to: 5) equals: #(3 4 5).
	
	self assert: (3.0 to: 5.0) equals: #(3.0 4.0 5.0).
	self assert: (3.0 to: 5.0 by: 0.5) equals: #(3.0 3.5 4.0 4.5 5.0).
	
	self assert: (3/1 to: 5/1) equals: #(3 4 5).

	"dynamic array, because fractions aren't literals"
	self assert: (1/2 to: 5/2 by: 1/2) equals: {1/2 . 1 . 3/2 . 2 . 5/2}.
]

{ #category : #tests }
IntervalTest >> testOtherNewFrom [
	self assert: (Interval newFrom: #(1 2 3)) equals: (1 to: 3).
	self assert: (Interval newFrom: #(33 5 -23)) equals: (33 to: -23 by: -28).
	self should: [ Interval newFrom: #(33 5 -22) ] raise: Error.
	self assert: (#(33 5 -23) as: Interval) equals: (33 to: -23 by: -28).
	self should: [ #(33 5 -22) as: Interval ] raise: Error.

	self assert: ((-4 to: -12 by: -1) as: Interval) equals: (-4 to: -12 by: -1).
	self assert: (Interval newFrom: (1 to: 1)) equals: (1 to: 1).
	self assert: (Interval newFrom: (1 to: 0)) equals: (1 to: 0).
	self assert: (#(1) as: Interval) equals: (1 to: 1).
	self assert: (#() as: Interval) equals: (1 to: 0)
]

{ #category : #tests }
IntervalTest >> testPermutationsDo [
	| i oc |
	i := 1.234 to: 4.234.
	oc := OrderedCollection new.
	i permutationsDo: [ :e | oc add: e ].
	self assert: oc size equals: i size factorial.
	^ oc
]

{ #category : #tests }
IntervalTest >> testRangeIncludes [
	self
		assert: ((1 to: 10)
				rangeIncludes: 3).
	self
		assert: ((1 to: 10 by: 2)
				rangeIncludes: 3).
	self
		deny: ((1 to: 10)
				rangeIncludes: 0).
	self
		deny: ((1 to: 10)
				rangeIncludes: 11).
	self
		deny: ((1 to: 10 by: 2)
				rangeIncludes: 0).
	self
		deny: ((1 to: 10 by: 2)
				rangeIncludes: 11)
]

{ #category : #tests }
IntervalTest >> testReverseDo [
	| s i |
	s := OrderedCollection new.
	i := 10 to: 20.
	i
		reverseDo: [:each | s addFirst: each].
	self
		assert: (s hasEqualElements: i)
]

{ #category : #tests }
IntervalTest >> testReverseUnevenDo [
	| s i |
	s := OrderedCollection new.
	i := 10 to: 20 by: 3.
	i
		reverseDo: [:each | s addFirst: each].
	self
		assert: (s hasEqualElements: i)
]

{ #category : #tests }
IntervalTest >> testShuffled [

	| i is |
	i := (1 to: 20).
	is := i shuffled.
	self assert: i ~= is.
	self assert: i size equals: is size.
	self assert: (is includesAll: i).
	self assert: (i includesAll: is).
]

{ #category : #tests }
IntervalTest >> testUnevenDo [
	| s i |
	s := OrderedCollection new.
	i := 10 to: 20 by: 3.
	i
		do: [:each | s addLast: each].
	self
		assert: (s hasEqualElements: i)
]

{ #category : #requirements }
IntervalTest >> unsortedCollection [
" retur a collection that is not yat sorted"
	^ (10 to: 1 by: -2)
]
