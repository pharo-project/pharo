"
SUnit tests for bags
"
Class {
	#name : #BagTest,
	#superclass : #CollectionRootTest,
	#traits : 'TAddTest + TIncludesWithIdentityCheckTest + TCloneTest + TCopyTest + TSetArithmetic + TConvertTest + TAsStringCommaAndDelimiterTest + TRemoveForMultiplenessTest + TPrintTest + TConvertAsSortedTest + TConvertAsSetForMultiplinessTest + TConcatenationTest + TStructuralEqualityTest + (TCreationWithTest - {#testOfSize}) + TOccurrencesForMultiplinessTest',
	#classTraits : 'TAddTest classTrait + TIncludesWithIdentityCheckTest classTrait + TCloneTest classTrait + TCopyTest classTrait + TSetArithmetic classTrait + TConvertTest classTrait + TAsStringCommaAndDelimiterTest classTrait + TRemoveForMultiplenessTest classTrait + TPrintTest classTrait + TConvertAsSortedTest classTrait + TConvertAsSetForMultiplinessTest classTrait + TConcatenationTest classTrait + TStructuralEqualityTest classTrait + TCreationWithTest classTrait + TOccurrencesForMultiplinessTest classTrait',
	#instVars : [
		'empty',
		'nonEmpty',
		'collectResult',
		'emptyButAllocatedWith20',
		'collectionWithElement',
		'collectionIn',
		'collectionNotIn',
		'collectionOfString',
		'elementNotIn',
		'collectionWithCharacters',
		'otherCollectionWithoutEqualElements',
		'collectionWithoutNilMoreThan5'
	],
	#category : #'Collections-Unordered-Tests-Bags'
}

{ #category : #'tests - includes' }
BagTest >> anotherElementNotIn [
	^ 42
]

{ #category : #requirements }
BagTest >> anotherElementOrAssociationIn [
	" return an element (or an association for Dictionary ) present  in 'collection' "
	^ self collection anyOne
]

{ #category : #requirements }
BagTest >> anotherElementOrAssociationNotIn [
	" return an element (or an association for Dictionary )not present  in 'collection' "
	^ elementNotIn
]

{ #category : #coverage }
BagTest >> classToBeTested [
	
	^ Bag
]

{ #category : #setup }
BagTest >> collection [

	^ nonEmpty.
]

{ #category : #'tests - set arithmetic' }
BagTest >> collectionClass [
	"Return the class to be used to create instances of the class tested"
	
	^ Bag
]

{ #category : #requirements }
BagTest >> collectionInForIncluding [
	 ^ collectionIn
]

{ #category : #requirements }
BagTest >> collectionMoreThan5Elements [
" return a collection including at least 5 elements"
	
	^ collectionWithoutNilMoreThan5
]

{ #category : #requirements }
BagTest >> collectionNotIncluded [
	^ collectionNotIn
]

{ #category : #requirements }
BagTest >> collectionOfFloat [
	^ collectionOfString
]

{ #category : #requirements }
BagTest >> collectionWithCharacters [
	^ collectionWithCharacters .
]

{ #category : #requirements }
BagTest >> collectionWithCopyNonIdentical [
	" return a collection that include elements for which 'copy' return a different object (this is not the case of SmallInteger)"
	^ collectionOfString
]

{ #category : #requirements }
BagTest >> collectionWithElement [
	"Returns a collection that already includes what is returned by #element."
	^ collectionWithElement
]

{ #category : #requirements }
BagTest >> collectionWithElementsToRemove [
	^ collectionIn
]

{ #category : #requirements }
BagTest >> collectionWithEqualElements [
	^ nonEmpty
]

{ #category : #requirements }
BagTest >> collectionWithSortableElements [
" return a collection elements that can be sorte ( understanding message ' < '  or ' > ')"
	^ nonEmpty
]

{ #category : #requirements }
BagTest >> collectionWithoutEqualElements [
	^ otherCollectionWithoutEqualElements
]

{ #category : #requirements }
BagTest >> collectionWithoutNilElements [
	" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	^ collectionWithoutNilMoreThan5
]

{ #category : #'tests - iterate' }
BagTest >> doWithoutNumber [

	^ 4
]

{ #category : #requirements }
BagTest >> element [
	^ super element
]

{ #category : #requirements }
BagTest >> elementInForIncludesTest [

	^ self element
]

{ #category : #requirements }
BagTest >> elementInForOccurrences [
" return an element included in nonEmpty"
	^self nonEmpty anyOne.
]

{ #category : #requirements }
BagTest >> elementNotIn [

	^elementNotIn
]

{ #category : #'tests - includes' }
BagTest >> elementNotInForOccurrences [
	^ 666
]

{ #category : #'tests - copying' }
BagTest >> elementToAdd [
	^ 42
]

{ #category : #requirements }
BagTest >> elementTwiceIn [
	^ super elementTwiceIn
]

{ #category : #requirements }
BagTest >> elementTwiceInForOccurrences [
" return an element included exactly two time in # collectionWithEqualElements"
^ self elementTwiceIn
]

{ #category : #requirements }
BagTest >> elementsCopyNonIdenticalWithoutEqualElements [
	" return a collection that does niot incllude equal elements ( classic equality )
	all elements included are elements for which copy is not identical to the element  "
	^ collectionOfString
]

{ #category : #setup }
BagTest >> empty [

	^ empty
]

{ #category : #setup }
BagTest >> emptyButAllocatedWith20 [ 
	
		^ emptyButAllocatedWith20
]

{ #category : #'tests - iterate' }
BagTest >> expectedSizeAfterReject [
	^ 2
]

{ #category : #requirements }
BagTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	^ nonEmpty
]

{ #category : #requirements }
BagTest >> integerCollectionWithoutEqualElements [
	^ otherCollectionWithoutEqualElements
]

{ #category : #setup }
BagTest >> nonEmpty [

	^ nonEmpty
]

{ #category : #requirements }
BagTest >> nonEmpty1Element [

	^ self speciesClass  new add: self element ;yourself.
]

{ #category : #requirements }
BagTest >> nonEmptyWithoutEqualElements [
" return a collection without equal elements "
	^ otherCollectionWithoutEqualElements
]

{ #category : #requirements }
BagTest >> otherCollection [
	^ otherCollectionWithoutEqualElements
]

{ #category : #setup }
BagTest >> result [

	^ collectResult.
]

{ #category : #requirements }
BagTest >> secondCollection [
" return a collection that will be the second part of the concatenation"
	^ collectionWithCharacters
]

{ #category : #requirements }
BagTest >> selectedNumber [
	^ 4
]

{ #category : #running }
BagTest >> setUp [
	super setUp.
	empty := self speciesClass new.
	nonEmpty := self speciesClass new
		add: 13;
		add: -2;
		add: self elementTwiceIn;
		add: 10;
		add: self elementTwiceIn;
		add: self element;
		yourself.
	elementNotIn := 0.
	collectionIn := self speciesClass new
		add: -2;
		add: self elementTwiceIn;
		add: 10;
		yourself.
	collectionNotIn := self speciesClass new
		add: self elementNotIn;
		add: 5;
		yourself.
	collectionOfString := self speciesClass new
		add: 'a';
		add: 'b';
		add: 'c';
		yourself.
	otherCollectionWithoutEqualElements := self speciesClass new
		add: 1;
		add: 20;
		add: 30;
		add: 40;
		yourself.
	collectionWithoutNilMoreThan5 := self speciesClass new
		add: 1;
		add: 2;
		add: 3;
		add: 4;
		add: 5;
		add: 6;
		yourself.
	collectResult := self speciesClass new
		add: SmallInteger;
		add: SmallInteger;
		add: SmallInteger;
		add: SmallInteger;
		add: SmallInteger;
		add: SmallInteger;
		yourself.
	emptyButAllocatedWith20 := self speciesClass new: 20.
	collectionWithElement := self speciesClass new
		add: self element;
		yourself.
	collectionWithCharacters := self speciesClass new
		add: $p;
		add: $v;
		add: $i;
		add: $y;
		yourself
]

{ #category : #setup }
BagTest >> sizeCollection [
	^ otherCollectionWithoutEqualElements
]

{ #category : #requirements }
BagTest >> speciesClass [
	
	^ Bag
]

{ #category : #'basic tests' }
BagTest >> testAdd [

	| aBag |
	aBag := Bag new.
	aBag add: 'a'.
	aBag add: 'b'.
	
	self assert: aBag size equals: 2.
	aBag add: 'a'.
	self assert: aBag size equals: 3.
	self assert: (aBag occurrencesOf: 'a') equals: 2
]

{ #category : #'basic tests' }
BagTest >> testAddWithOccurrences [
	| aBag |
	aBag := Bag new.
	aBag add: 'a' withOccurrences: 3.
	self assert: aBag size equals: 3
]

{ #category : #'tests - iterating' }
BagTest >> testAnySastify [

	self assert: ( self collection anySatisfy: [:each | each = self element]).
	self deny: (self collection anySatisfy: [:each | each isString]).
]

{ #category : #'basic tests' }
BagTest >> testAsBag [
	| aBag |
	aBag := Bag new.

	self assert: aBag asBag equals: aBag
]

{ #category : #'tests - as string comma delimiter sequenceable' }
BagTest >> testAsCommaStringEmpty [

	self assert: self empty asCommaString = ''.
	self assert: self empty asCommaStringAnd = ''.
]

{ #category : #'tests - as string comma delimiter sequenceable' }
BagTest >> testAsCommaStringOne [
	
	self nonEmpty1Element do: 
		[:each | 
		self assert: each asString =self nonEmpty1Element  asCommaString.
		self assert: each asString=self nonEmpty1Element  asCommaStringAnd.].
]

{ #category : #'basic tests' }
BagTest >> testAsDictionary [
	| aBag aDictonary |
	aBag := Bag new.
	aBag add: 'a' withOccurrences: 4.
	aBag add: 'b' withOccurrences: 2.
	aDictonary := aBag asDictionary.
	self assert: aDictonary size equals: 2.
	self assert: (aBag as: Dictionary) equals: aDictonary.
	self assert: (aDictonary at: 'a') equals: 4.
	self assert: aBag asDictionary equals: aBag valuesAndCounts.
	self deny: aBag asDictionary identicalTo: aBag valuesAndCounts
]

{ #category : #'tests - converting' }
BagTest >> testAsIdentitySet [
	"Test with a collection without equal elements :"

	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: IdentitySet
]

{ #category : #'basic tests' }
BagTest >> testAsSet [
	| aBag aSet |
	aBag := Bag new.
	aBag add: 'a' withOccurrences: 4.
	aBag add: 'b' withOccurrences: 2.
	aSet := aBag asSet.
	self assert: aSet size equals: 2.
	self assert: (aSet occurrencesOf: 'a') equals: 1
]

{ #category : #'tests - as sorted collection' }
BagTest >> testAsSortedCollectionWithSortBlock [

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

{ #category : #'tests - as string comma delimiter sequenceable' }
BagTest >> testAsStringOnDelimiterEmpty [
	self assert: (String streamContents: [ :emptyStream | self empty asStringOn: emptyStream delimiter: ', ' ]) equals: ''
]

{ #category : #'tests - as string comma delimiter sequenceable' }
BagTest >> testAsStringOnDelimiterLastOne [

	| delim oneItemStream result |
	
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream  do:
		[:each1 |
		self nonEmpty1Element do: [:each2 |self assert: each1 = (each2 asString) ]
		 ].
]

{ #category : #'tests - as string comma delimiter sequenceable' }
BagTest >> testAsStringOnDelimiterOne [

	| delim oneItemStream result |

	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim.
	oneItemStream  do:
		[:each1 |
		self nonEmpty1Element do: [:each2 |self assert: each1 = (each2 asString) ]
		 ].
]

{ #category : #'basic tests' }
BagTest >> testCopy [
	
	| aBag newBag |
	aBag := Bag new.
	aBag add:'a' withOccurrences: 4.
	aBag add:'b' withOccurrences: 2.
	newBag := aBag copy.
	self assert: newBag equals: newBag.
	self assert: newBag asSet size equals: 2
]

{ #category : #'tests - copy - clone' }
BagTest >> testCopyCreatesNewObject [
	 
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: self nonEmpty == copy.
]

{ #category : #'tests - copy' }
BagTest >> testCopyNonEmptyWithout [
	
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
BagTest >> testCopyNonEmptyWithoutAllNotIncluded [
]

{ #category : #tests }
BagTest >> testCreation [
	| bag |
	bag := Bag new.
	self assert: bag size equals: 0.
	self assertEmpty: bag
]

{ #category : #tests }
BagTest >> testCumulativeCounts [
	| bag cumulativeCounts |
	bag := Bag new.
	bag add: '1' withOccurrences: 50.
	bag add: '2' withOccurrences: 40.
	bag add: '3' withOccurrences: 10.
	
	cumulativeCounts := bag cumulativeCounts.
	
	self assert: cumulativeCounts size equals: 3.
	self assert: cumulativeCounts first equals: (50 -> '1').
	self assert: cumulativeCounts second equals: (90 -> '2').
	self assert: cumulativeCounts third equals: (100 -> '3')
]

{ #category : #tests }
BagTest >> testEqual [
	| bag1 bag2 |
	bag1 := Bag new.
	bag2 := Bag new.
	self assert: bag1 equals: bag2.
	bag1
		add: #a;
		add: #b.
	bag2
		add: #a;
		add: #a.
	self deny: bag1 equals: bag2.
	self assert: bag1 equals: bag1.
	bag1 add: #a.
	bag2 add: #b.
	self assert: bag1 equals: bag2.
	bag1 add: #c.
	self deny: bag1 equals: bag2.
	bag2 add: #c.
	self assert: bag1 equals: bag2
]

{ #category : #'tests - equality' }
BagTest >> testEqualSignOfIdenticalCollectionObjects [
	
	self assert: self empty equals: self empty. 
	self assert: self nonEmpty equals: self nonEmpty
]

{ #category : #tests }
BagTest >> testFlatCollect [
	| bag |
	bag := Bag new.
	bag add: {1 . 2 . 3}.
	bag add: {4 . 5 . 6}.

	self assert: (bag flatCollect: [ :x | x ]) equals: #(1 2 3 4 5 6) asBag.
	self assert: (bag flatCollect: [ :x | x ]) class identicalTo: Bag.
	self assertEmpty: (#() asBag flatCollect: [ :x | 1 to: 4 ])
]

{ #category : #'tests - includes' }
BagTest >> testIdentityIncludes [
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "

	" can not be used by collections that can't include elements for wich copy doesn't return another instance "

	| collection anElement |
	self collectionWithCopyNonIdentical.
	collection := self collectionWithCopyNonIdentical.
	anElement := collection anyOne copy.	"self assert: (collection includes: element)."
	self deny: (collection identityIncludes: anElement)
]

{ #category : #'tests - includes' }
BagTest >> testIdentityIncludesNonSpecificComportement [
	"Test the same comportement than #includes:"
	
	| collection |	
	collection := self nonEmpty.
	
	self deny: (collection identityIncludes: self elementNotIn).
	self assert: (collection identityIncludes: collection anyOne)
]

{ #category : #'tests - includes' }
BagTest >> testIncludesElementIsNotThere [
	
	self deny: (self nonEmpty includes: self elementNotInForOccurrences).
	self assert: (self nonEmpty includes: self nonEmpty anyOne).
	self deny: (self empty includes: self elementNotInForOccurrences)
]

{ #category : #'tests - set arithmetic' }
BagTest >> testIntersectionItself [
	
	| result |
	result := self collectionWithoutEqualElements intersection: self collectionWithoutEqualElements.
	self assert: result size equals: self collectionWithoutEqualElements size.
	self collectionWithoutEqualElements do: [ :each|
		self assert: (result includes: each) ].
]

{ #category : #'basic tests' }
BagTest >> testOccurrencesOf [	

	| aBag |
 	aBag := Bag new.	
	aBag add: 'a' withOccurrences: 3.
	aBag add: 'b'.
	aBag add: 'b'.
	aBag add: 'b'.
	aBag add: 'b'.	
	self assert: (aBag occurrencesOf: 'a') equals: 3.
	self assert: (aBag occurrencesOf: 'b') equals: 4.
	self assert: (aBag occurrencesOf: 'c') equals: 0.
	self assert: (aBag occurrencesOf: nil) equals: 0.
	aBag add: nil.
	self assert: (aBag occurrencesOf: nil) equals: 1.
]

{ #category : #'tests - occurrencesOf for multipliness' }
BagTest >> testOccurrencesOfForMultipliness [

	| collection elem |
	collection := self collectionWithEqualElements.
	elem := self elementTwiceInForOccurrences.

	self assert: (collection occurrencesOf: elem ) equals: 2.
]

{ #category : #tests }
BagTest >> testRemove [
	| bag item |
	item := 'test item'.
	bag := Bag new.

	bag add: item.
	self assert: bag size equals: 1.
	bag remove: item.
	self assertEmpty: bag.

	bag add: item withOccurrences: 2.
	bag remove: item.
	bag remove: item.
	self assert: bag size equals: 0.

	self should: [ bag remove: item ] raise: Error
]

{ #category : #tests }
BagTest >> testRemoveAll [
	"Allows one to remove all elements of a collection"

	| c1 c2 s2 |
	c1 := #(10 9 8 7 5 4 4 2) asBag.
	c2 := c1 copy.
	s2 := c2 size.

	c1 removeAll.

	self assert: c1 size equals: 0.
	self assert: c2 size = s2 description: 'the copy has not been modified'
]

{ #category : #'tests - remove' }
BagTest >> testRemoveElementThatExistsTwice [
	| size |
	size := self nonEmpty size.
	self assert: (self nonEmpty includes: self elementTwiceIn).
	self nonEmpty remove: self elementTwiceIn.
	self assert: size - 1 equals: self nonEmpty size
]

{ #category : #tests }
BagTest >> testSortedCounts [
	
	| bag sortedCounts|
	bag := Bag new.
	bag add: '1' withOccurrences: 10.
	bag add: '2' withOccurrences: 1.
	bag add: '3' withOccurrences: 5.
	
	sortedCounts := bag sortedCounts.
	self assert: sortedCounts size equals: 3.
	
	self assert: sortedCounts first equals: (10->'1').
	self assert: sortedCounts second equals: (5->'3').
	self assert: sortedCounts third equals: (1->'2')
]

{ #category : #tests }
BagTest >> testSortedElements [
	
	| bag sortedElements|
	bag := Bag new.
	bag add: '2' withOccurrences: 1.
	bag add: '1' withOccurrences: 10.
	bag add: '3' withOccurrences: 5.
	
	sortedElements := bag sortedElements.
	
	self assert: sortedElements size equals: 3.
	
	self assert: sortedElements first equals: ('1'->10).
	self assert: sortedElements second equals: ('2'->1).
	self assert: sortedElements third equals: ('3'->5)
]

{ #category : #'tests - adding' }
BagTest >> testTAdd [
	| added collection |
	collection := self otherCollection.
	added := collection add: self element.
	
	self assert: added == self element.	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection includes: self element)	.
	self assert: (self collectionWithElement includes: self element).
]

{ #category : #'tests - adding' }
BagTest >> testTAddIfNotPresentWithNewElement [

	| added oldSize collection elem |
	collection := self otherCollection.
	oldSize := collection size.
	elem := self element.
	self deny: (collection includes: elem ).
	
	added := collection  addIfNotPresent: elem .
	self assert: added == elem . "test for identiy because #add: has not reason to copy its parameter."
	self assert: collection size equals: (oldSize + 1).
]

{ #category : #'tests - creation' }
BagTest >> testWithWith [
	
	| aCol collection element1 element2 |
	collection := self collectionMoreThan5Elements asOrderedCollection copyFrom: 1 to: 2.
	element1 := collection at: 1.
	element2 := collection at: 2.
	
	aCol := self collectionClass with: element1  with: element2 .
	self assert: (aCol occurrencesOf: element1 ) equals: (collection occurrencesOf: element1).
	self assert: (aCol occurrencesOf: element2 ) equals: (collection occurrencesOf: element2)
]

{ #category : #requirements }
BagTest >> withEqualElements [
	" return a collection  including equal elements (classic equality)"
	^ nonEmpty .
]
