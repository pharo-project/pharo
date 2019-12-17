"
A set of test cases which thoroughly test functionality of the LinkedList class.
"
Class {
	#name : #LinkedListTest,
	#superclass : #TestCase,
	#traits : 'TAddTest - {#testTAddWithOccurences. #testTAddTwice. #testTWriteTwice} + TEmptyTest + TIterateTest + TIterateSequencedReadableTest + TPrintTest + TAsStringCommaAndDelimiterSequenceableTest + TIndexAccess + TSequencedElementAccessTest + TSubCollectionAccess + (TConvertTest - {#testAsByteArray. #integerCollectionWithoutEqualElements}) + (TCopyPartOfSequenceable - {#testCopyEmptyMethod}) + (TCopySequenceableSameContents - {#testSortBy. #integerCollection}) + TCopySequenceableWithOrWithoutSpecificElements + TCopyTest + (TCopySequenceableWithReplacement - {#testCopyReplaceAllWithManyOccurence. #collectionWith2TimeSubcollection}) + TBeginsEndsWith + TRemoveTest + TSetArithmetic + TIncludesWithIdentityCheckTest + TStructuralEqualityTest + TOccurrencesTest + TSortTest',
	#classTraits : 'TAddTest classTrait + TEmptyTest classTrait + TIterateTest classTrait + TIterateSequencedReadableTest classTrait + TPrintTest classTrait + TAsStringCommaAndDelimiterSequenceableTest classTrait + TIndexAccess classTrait + TSequencedElementAccessTest classTrait + TSubCollectionAccess classTrait + TConvertTest classTrait + TCopyPartOfSequenceable classTrait + TCopySequenceableSameContents classTrait + TCopySequenceableWithOrWithoutSpecificElements classTrait + TCopyTest classTrait + TCopySequenceableWithReplacement classTrait + TBeginsEndsWith classTrait + TRemoveTest classTrait + TSetArithmetic classTrait + TIncludesWithIdentityCheckTest classTrait + TStructuralEqualityTest classTrait + TOccurrencesTest classTrait + TSortTest classTrait',
	#instVars : [
		'nextLink',
		'n',
		'list',
		'link1',
		'link2',
		'link3',
		'link4',
		'nonEmpty',
		'otherList',
		'link',
		'collectionWithoutNil',
		'nonEmpty1Element',
		'collectionWithoutEqualElements',
		'elementNotIn',
		'elementIn',
		'collection5Elements',
		'collectResult',
		'unsortedCollection',
		'sortedCollection'
	],
	#category : #'Collections-Tests-Sequenceable'
}

{ #category : #requirements }
LinkedListTest >> accessCollection [
	^collectionWithoutEqualElements 
]

{ #category : #requirements }
LinkedListTest >> anotherElementNotIn [
" return an element included  in 'collection' "
	^ elementNotIn 
]

{ #category : #requirements }
LinkedListTest >> anotherElementOrAssociationIn [
	" return an element (or an association for Dictionary ) present  in 'collection' "
	^ self collection anyOne
]

{ #category : #requirements }
LinkedListTest >> anotherElementOrAssociationNotIn [
	" return an element (or an association for Dictionary )not present  in 'collection' "
	^ elementNotIn 
]

{ #category : #requirements }
LinkedListTest >> collection [
	^ self nonEmpty
]

{ #category : #requirements }
LinkedListTest >> collectionClass [
	"Return the class to be used to create instances of the class tested"
	
	^ LinkedList
]

{ #category : #requirements }
LinkedListTest >> collectionMoreThan1NoDuplicates [
	" return a collection of size 5 without equal elements"
	^ collectionWithoutEqualElements
]

{ #category : #requirements }
LinkedListTest >> collectionNotIncluded [
" return a collection for wich each element is not included in 'nonEmpty' "
	^ collectionWithoutNil 
]

{ #category : #requirements }
LinkedListTest >> collectionWith1TimeSubcollection [
" return a collection including 'oldSubCollection'  only one time "
	^ self oldSubCollection 
]

{ #category : #requirements }
LinkedListTest >> collectionWithCopyNonIdentical [
	" return a collection that include elements for which 'copy' return a different object (this is not the case of SmallInteger)"
	^ collectionWithoutEqualElements
]

{ #category : #requirements }
LinkedListTest >> collectionWithElement [
	"Returns a collection that already includes what is returned by #element."
	^ self collection
]

{ #category : #requirements }
LinkedListTest >> collectionWithElementsToRemove [
" return a collection of elements included in 'nonEmpty'  "
	^ self nonEmpty 
]

{ #category : #requirements }
LinkedListTest >> collectionWithSortableElements [
	" return a collection only including elements that can be sorted (understanding '<' )"
	^ collection5Elements 
]

{ #category : #requirements }
LinkedListTest >> collectionWithoutEqualElements [
	" return a collection not including equal elements "

	^ collectionWithoutEqualElements
]

{ #category : #requirements }
LinkedListTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element "
	^collectionWithoutNil 
]

{ #category : #requirements }
LinkedListTest >> element [
	^ link ifNil: [link := ValueLink value: 42. "so that we can recognize this link"]
]

{ #category : #requirements }
LinkedListTest >> elementInForElementAccessing [
" return an element inculded in 'accessCollection '"
	^ elementIn 
]

{ #category : #requirements }
LinkedListTest >> elementInForIndexAccessing [
" return an element included in 'accessCollection' "
	^ elementIn 
]

{ #category : #requirements }
LinkedListTest >> elementNotIn [
	^ Link new
]

{ #category : #requirements }
LinkedListTest >> elementNotInForElementAccessing [
" return an element not included in 'accessCollection' "
	^ elementNotIn 
]

{ #category : #requirements }
LinkedListTest >> elementNotInForIndexAccessing [
" return an element not included in 'accessCollection' "
	^ elementNotIn 
]

{ #category : #requirements }
LinkedListTest >> elementNotInForOccurrences [
" return an element notIncluded in #collectionWithoutEqualElements"
	^ elementNotIn 
]

{ #category : #requirements }
LinkedListTest >> elementToAdd [
" return an element of type 'nonEmpy' elements'type'"
	^ ValueLink value: 77
]

{ #category : #'tests - empty' }
LinkedListTest >> empty [
	^ list
]

{ #category : #requirements }
LinkedListTest >> indexInForCollectionWithoutDuplicates [
" return an index between 'collectionWithoutEqualsElements'  bounds"
	^ 2
]

{ #category : #requirements }
LinkedListTest >> indexInNonEmpty [
	"Return an index between bounds of 'nonEmpty'"

	^ self nonEmpty size
]

{ #category : #requirements }
LinkedListTest >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	^ collectionWithoutEqualElements 
]

{ #category : #requirements }
LinkedListTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	^ collectionWithoutEqualElements 
]

{ #category : #accessing }
LinkedListTest >> n [
	^n
]

{ #category : #accessing }
LinkedListTest >> n: number [
	n := number.
	
]

{ #category : #accessing }
LinkedListTest >> nextLink [
	^nextLink
]

{ #category : #accessing }
LinkedListTest >> nextLink: aLink [
	nextLink := aLink
]

{ #category : #requirements }
LinkedListTest >> nonEmpty [
	^ nonEmpty ifNil: [nonEmpty := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1 with: self element]
]

{ #category : #requirements }
LinkedListTest >> nonEmpty1Element [
" return a collection of size 1 including one element"
	^ nonEmpty1Element 
]

{ #category : #requirements }
LinkedListTest >> nonEmptyMoreThan1Element [
" return a collection that don't includes equl elements'" 
	^collectionWithoutNil 
]

{ #category : #requirements }
LinkedListTest >> nonEmptyWithoutEqualElements [
" return a collection without equal elements "
	^ collectionWithoutEqualElements 
]

{ #category : #requirements }
LinkedListTest >> oldSubCollection [
" return a subCollection included in collectionWith1TimeSubcollection .
ex :   subCollection := #( 2 3 4) and collectionWith1TimeSubcollection := #(1 2 3 4 5)"
	^ self nonEmpty 
]

{ #category : #requirements }
LinkedListTest >> otherCollection [
	^ otherList ifNil: [otherList := LinkedList with: Link new with: Link new]
]

{ #category : #requirements }
LinkedListTest >> replacementCollection [
" return a collection that will be used to replace 'oldSubcollection' in ' collectionWith1TimeSubcollection'  " 
	^ collectionWithoutNil 
]

{ #category : #requirements }
LinkedListTest >> result [
	"Returns a collection of the classes of elements in #collection"
	 ^ collectResult
]

{ #category : #running }
LinkedListTest >> setUp [
	
	super setUp.
	list := LinkedList new.
	link1 := 133.
	link2 := 'test'.
	link3 := $h.
	link4 := Set new.
	elementNotIn := Link new.
	collectionWithoutNil := LinkedList new add: link1; add: link2 ; add: link3; yourself.
	elementIn := 'thisElementIsIncluded'.
	collectionWithoutEqualElements := LinkedList new add: elementIn ; add: 'pewpew' ; add: 'normal links'; add: 'are no fun!' ;add: $x ;yourself.
	collection5Elements := collectionWithoutEqualElements .
	unsortedCollection := LinkedList new addAll: #(57 937 92 49 0 78 -87); yourself.
	sortedCollection := LinkedList new addAll: #(-87 0 49 57 78 92 937); yourself.
	"sameAttendAndBegining := LinkedList new add: Link new; add: Link new ; add: Link new; yourself."
	link := ValueLink value: 42.
	nonEmpty1Element :=  LinkedList new add: Link new; yourself.
	 "so that we can recognize this link"
	"nonEmpty := LinkedList with: link with: Link new."
	"otherList := LinkedList with: Link new with: Link new."

]

{ #category : #requirements }
LinkedListTest >> sortedInAscendingOrderCollection [
	"Return a collection sorted in ascending order"
	^ sortedCollection
	
]

{ #category : #requirements }
LinkedListTest >> speciesClass [
	
	^LinkedList
]

{ #category : #requirements }
LinkedListTest >> subCollectionNotIn [
" return a collection for which at least one element is not included in 'accessCollection' "
	^ collectionWithoutNil 
]

{ #category : #running }
LinkedListTest >> tearDown [
	list := nil.
	link1 := nil.
	link2 := nil.
	link3 := nil.
	link4 := nil.
	
	link := nil.
	nonEmpty := nil.
	otherList := nil.
	
	super tearDown
]

{ #category : #tests }
LinkedListTest >> test01add [
	self assertEmpty: list.
	list add: link1.
	self assert: list size equals: 1.
	self assert: list first equals: link1.

	list add: link2.
	self assert: list size equals: 2.
	self assert: list first equals: link1.
	self assert: list second equals: link2.

	list add: link3.
	self assert: list size equals: 3.
	self assert: list first equals: link1.
	self assert: list second equals: link2.
	self assert: list third equals: link3.

	list add: link4.
	self assert: list size equals: 4.
	self assert: list first equals: link1.
	self assert: list second equals: link2.
	self assert: list third equals: link3.
	self assert: list fourth equals: link4
]

{ #category : #tests }
LinkedListTest >> test02addLast [
	self assertEmpty: list.

	list addLast: link1.
	self assert: list size equals: 1.
	self assert: list first equals: link1.

	list addLast: link2.
	self assert: list size equals: 2.
	self assert: list first equals: link1.
	self assert: list second equals: link2.

	list addLast: link3.
	self assert: list size equals: 3.
	self assert: list first equals: link1.
	self assert: list second equals: link2.
	self assert: list third equals: link3.

	list addLast: link4.
	self assert: list size equals: 4.
	self assert: list first equals: link1.
	self assert: list second equals: link2.
	self assert: list third equals: link3.
	self assert: list fourth equals: link4
]

{ #category : #tests }
LinkedListTest >> test03addFirst [
	self assertEmpty: list.

	list addFirst: link1.
	self assert: list size equals: 1.
	self assert: list first equals: link1.

	list addFirst: link2.
	self assert: list size equals: 2.
	self assert: list first equals: link2.
	self assert: list second equals: link1.

	list addFirst: link3.
	self assert: list size equals: 3.
	self assert: list first equals: link3.
	self assert: list second equals: link2.
	self assert: list third equals: link1.

	list addFirst: link4.
	self assert: list size equals: 4.
	self assert: list first equals: link4.
	self assert: list second equals: link3.
	self assert: list third equals: link2.
	self assert: list fourth equals: link1
]

{ #category : #tests }
LinkedListTest >> test04addBefore [
	self assertEmpty: list.

	list add: link1.
	self assert: list size equals: 1.
	self assert: list first identicalTo: link1.

	list add: link2 before: link1.
	self assert: list size equals: 2.
	self assert: list first identicalTo: link2.
	self assert: list second identicalTo: link1.

	list add: link3 before: link1.
	self assert: list size equals: 3.
	self assert: list first identicalTo: link2.
	self assert: list second identicalTo: link3.
	self assert: list third identicalTo: link1.

	list add: link4 before: link1.
	self assert: list size equals: 4.
	self assert: list first identicalTo: link2.
	self assert: list second identicalTo: link3.
	self assert: list third identicalTo: link4.
	self assert: list fourth identicalTo: link1
]

{ #category : #tests }
LinkedListTest >> test05addBefore [
	self assertEmpty: list.

	list add: link1.
	self assert: list size equals: 1.
	self assert: list first identicalTo: link1.

	list add: link2 before: link1.
	self assert: list size equals: 2.
	self assert: list first identicalTo: link2.
	self assert: list second identicalTo: link1.

	list add: link3 before: link2.
	self assert: list size equals: 3.
	self assert: list first identicalTo: link3.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link1.

	list add: link4 before: link3.
	self assert: list size equals: 4.
	self assert: list first identicalTo: link4.
	self assert: list second identicalTo: link3.
	self assert: list third identicalTo: link2.
	self assert: list fourth identicalTo: link1
]

{ #category : #tests }
LinkedListTest >> test06addAfter [
	self assertEmpty: list.

	list add: link1.
	self assert: list size equals: 1.
	self assert: list first identicalTo: link1.

	list add: link2 after: link1.
	self assert: list size equals: 2.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.

	list add: link3 after: link2.
	self assert: list size equals: 3.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link3.

	list add: link4 after: link3.
	self assert: list size equals: 4.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link3.
	self assert: list fourth identicalTo: link4
]

{ #category : #tests }
LinkedListTest >> test07addAfter [
	self assertEmpty: list.

	list add: link1.
	self assert: list size equals: 1.
	self assert: list first identicalTo: link1.

	list add: link2 after: link1.
	self assert: list size equals: 2.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.

	list add: link3 after: link1.
	self assert: list size equals: 3.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link3.
	self assert: list third identicalTo: link2.

	list add: link4 after: link1.
	self assert: list size equals: 4.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link4.
	self assert: list third identicalTo: link3.
	self assert: list fourth identicalTo: link2
]

{ #category : #tests }
LinkedListTest >> test08addAfter [
	| l first |
	l := LinkedList new.
	first := self class new n: 1.

	l add: first.
	l add: (self class new n: 3).

	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 3).

	l add: (self class new n: 2) after: first.

	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2 3)
]

{ #category : #tests }
LinkedListTest >> test09addAfter [
	| l last |
	l := LinkedList new.
	last := self class new n: 2.
	l add: (self class new n: 1).
	l add: last.

	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2).

	l add: (self class new n: 3) after: last.

	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2 3)
]

{ #category : #'tests - fixture' }
LinkedListTest >> test0FixtureIndexAccessTest [
	| res |
	self collectionMoreThan1NoDuplicates.
	self assert: self collectionMoreThan1NoDuplicates size equals: 5.
	res := true.
	self collectionMoreThan1NoDuplicates detect: [ :each | (self collectionMoreThan1NoDuplicates occurrencesOf: each) > 1 ] ifNone: [ res := false ].
	self assert: res equals: false.
	self elementInForIndexAccessing.
	self assert: (self collectionMoreThan1NoDuplicates includes: self elementInForIndexAccessing).
	self elementNotInForIndexAccessing.
	self deny: (self collectionMoreThan1NoDuplicates includes: self elementNotInForIndexAccessing)
]

{ #category : #tests }
LinkedListTest >> test10removeFirst [
	list add: link1.
	list add: link2.
	list add: link3.
	list add: link4.

	self assert: list size equals: 4.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link3.
	self assert: list fourth identicalTo: link4.

	list removeFirst.
	self assert: list size equals: 3.
	self assert: list first identicalTo: link2.
	self assert: list second identicalTo: link3.
	self assert: list third identicalTo: link4.

	list removeFirst.
	self assert: list size equals: 2.
	self assert: list first identicalTo: link3.
	self assert: list second identicalTo: link4.

	list removeFirst.
	self assert: list size equals: 1.
	self assert: list first identicalTo: link4.

	list removeFirst.
	self assertEmpty: list
]

{ #category : #tests }
LinkedListTest >> test11removeLast [
	list add: link1.
	list add: link2.
	list add: link3.
	list add: link4.

	self assert: list size equals: 4.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link3.
	self assert: list fourth identicalTo: link4.

	list removeLast.
	self assert: list size equals: 3.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link3.

	list removeLast.
	self assert: list size equals: 2.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.

	list removeLast.
	self assert: list size equals: 1.
	self assert: list first identicalTo: link1.

	list removeFirst.
	self assertEmpty: list
]

{ #category : #tests }
LinkedListTest >> test12remove [
	list add: link1.
	list add: link2.
	list add: link3.
	list add: link4.

	self assert: list size equals: 4.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link3.
	self assert: list fourth identicalTo: link4.

	list remove: link3.
	self assert: list size equals: 3.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link4.

	list remove: link2.
	self assert: list size equals: 2.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link4.

	list remove: link1.
	self assert: list size equals: 1.
	self assert: list first identicalTo: link4.

	list remove: link4.
	self assertEmpty: list
]

{ #category : #tests }
LinkedListTest >> test13remove [
	list add: link1.
	list add: link2.
	list add: link3.
	list add: link4.

	self assert: list size equals: 4.
	self assert: list first identicalTo: link1.
	self assert: list second identicalTo: link2.
	self assert: list third identicalTo: link3.
	self assert: list fourth identicalTo: link4.

	list remove: link1.
	self assert: list size equals: 3.
	self assert: list first identicalTo: link2.
	self assert: list second identicalTo: link3.
	self assert: list third identicalTo: link4.

	list remove: link4.
	self assert: list size equals: 2.
	self assert: list first identicalTo: link2.
	self assert: list second identicalTo: link3.

	list remove: link2.
	self assert: list size equals: 1.
	self assert: list first identicalTo: link3.

	list remove: link3.
	self assertEmpty: list
]

{ #category : #tests }
LinkedListTest >> test14removeIfAbsent [
	list add: link1.

	self assert: list size equals: 1.
	self assert: list first identicalTo: link1.

	list remove: link1.
	self assertEmpty: list.

	[ list remove: link1 ]
		on: Error
		do: [ ^ self ].

	"The execution should not get here. If yes, something went wrong."
	self fail
]

{ #category : #tests }
LinkedListTest >> test22addAll [
	| link5 link6 link7 link8 listToBeAdded |
	link5 := Link new.
	link6 := Link new.
	link7 := Link new.
	link8 := Link new.
	
	list
		add: link1;
		add: link2;
		add: link3;
		add: link4.
		
	listToBeAdded := LinkedList new.
	listToBeAdded
		add: link5;
		add: link6;
		add: link7;
		add: link8.
		
	list addAll: listToBeAdded.
	
	self should: [(list at: 1) == link1].
	self should: [(list at: 2) == link2].
	self should: [(list at: 3) == link3].
	self should: [(list at: 4) == link4].
	self should: [(list at: 5) == link5].
	self should: [(list at: 6) == link6].
	self should: [(list at: 7) == link7].
	self should: [(list at: 8) == link8].
]

{ #category : #tests }
LinkedListTest >> testAddAfter [
	| l first |
	l := LinkedList new.
	first := self class new n: 1.

	l add: first.
	l add: (self class new n: 3).
	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 3).
	l add: (self class new n: 2) after: first.
	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2 3)
]

{ #category : #tests }
LinkedListTest >> testAddAfterLast [
	| l last |
	l := LinkedList new.
	last := self class new n: 2.
	l add: (self class new n: 1).
	l add: last.
	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2).
	l add: (self class new n: 3) after: last.
	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2 3)
]

{ #category : #tests }
LinkedListTest >> testAddAfterLast2 [
	"LinkedListTest new testAddAfterLast2"

	| l first second third fourth |
	l := LinkedList new.
	first := self class new n: 1.
	second := self class new n: 2.
	third := self class new n: 3.
	fourth := self class new n: 4.
	l addLast: first.
	l addLast: second.
	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2).
	l add: third after: second.
	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2 3).
	l addLast: fourth.
	self assert: (l collect: [ :e | e n ]) asArray equals: #(1 2 3 4)
]

{ #category : #'tests - sequenceable' }
LinkedListTest >> testAtPut [
	| ll |
	ll := LinkedList new.
	ll add: 1.
	ll at: 1 put: 2.
	self assert: (ll at: 1) equals: 2
]

{ #category : #'tests - sequenceable' }
LinkedListTest >> testAtPutOutsideBounds [
	| ll |
	ll := LinkedList new.
	self should: [ ll at: 1 put: 1 ] raise: SubscriptOutOfBounds
]

{ #category : #'tests - creating' }
LinkedListTest >> testCreateAs [
	"Test that a LinkedList can be created by sending message #as: to another collection.
	Implementation note: this method is generic for sequenceable collection and should be traitified."
	| anotherCollection aLinkedList |
	anotherCollection := 1 to: 10.
	aLinkedList := anotherCollection as: LinkedList.
	self assert: (aLinkedList isMemberOf: LinkedList).
	self assert: aLinkedList size equals: anotherCollection size.
	aLinkedList with: anotherCollection do: [:nextElementOfLinkedList :nextElementOfAnotherCollection |
		self assert: nextElementOfLinkedList equals: nextElementOfAnotherCollection]
]

{ #category : #'tests - includes' }
LinkedListTest >> testIdentityIncludes [
	" test the comportement in presence of elements 'includes' but not 'identityIncludes' "

	" can not be used by collections that can't include elements for wich copy doesn't return another instance "

	| collection element |
	self collectionWithCopyNonIdentical.
	collection := self collectionWithCopyNonIdentical.
	element := collection anyOne copy.	"self assert: (collection includes: element)."
	self deny: (collection identityIncludes: element)
]

{ #category : #tests }
LinkedListTest >> testRemoveAll [
	| list2 |
	list add: link1.
	list add: link2.
	list add: link3.
	list add: link4.
	list2 := list copy.
	list removeAll.

	self assert: list size equals: 0.
	self assert: list2 size = 4 description: 'the copy has not been modified'
]

{ #category : #requirements }
LinkedListTest >> testSort [
	| aLinkedList |
	aLinkedList := LinkedList with: 1 with: 2.
	aLinkedList sort: [ :a :b | a > b ].
	self assert: aLinkedList equals: (LinkedList with: 2 with: 1)
]

{ #category : #'tests - swap' }
LinkedListTest >> testSwapAdjacent [
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 3 with: 4.
	self assert: (aList at: 3) equals: 2.
	self assert: (aList at: 4) equals: 3.
]

{ #category : #'tests - swap' }
LinkedListTest >> testSwapAdjacentEnd [
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 4 with: 5.
	self assert: (aList at: 4) equals: 1.
	self assert: (aList at: 5) equals: 2.
	self assert: aList last equals: 2.
]

{ #category : #'tests - swap' }
LinkedListTest >> testSwapAdjacentStart [
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 1 with: 2.
	self assert: (aList at: 1) equals: 4.
	self assert: (aList at: 2) equals: 5.
	self assert: aList first equals: 4.
]

{ #category : #'tests - swap' }
LinkedListTest >> testSwapBasic [
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 2 with: 4.
	self assert: (aList at: 2) equals: 2.
	self assert: (aList at: 4) equals: 4.

]

{ #category : #'tests - swap' }
LinkedListTest >> testSwapStartAndEnd [
	|aList|
	aList := LinkedList with: 5 with: 4 with: 3 with: 2 with: 1.
	
	aList swap: 1 with: 5.
	self assert: (aList at: 1) equals: 1.
	self assert: (aList at: 5) equals: 5.
	self assert: aList first equals: 1.
	self assert: aList last equals: 5.
]

{ #category : #'tests - adding' }
LinkedListTest >> testTAdd [
	| added |
	added := self otherCollection add: self element.
	self assert: added equals: self element.	"equality or identity ?"
	self assert: (self otherCollection includes: self element)
]

{ #category : #requirements }
LinkedListTest >> unsortedCollection [
"Return a collection that is not yet sorted"
	^ unsortedCollection
]
