"
A set of test cases which thoroughly test functionality of the LinkedList class.
"
Class {
	#name : #LinkedListTest,
	#superclass : #TestCase,
	#traits : 'TAddTest - {#testTAddWithOccurrences. #testTAddTwice. #testTWriteTwice} + TEmptyTest + TIterateTest + TIterateSequencedReadableTest + TPrintTest + TAsStringCommaAndDelimiterSequenceableTest + TIndexAccess + TSequencedElementAccessTest + TSubCollectionAccess + (TConvertTest - {#testAsByteArray. #integerCollectionWithoutEqualElements}) + (TCopyPartOfSequenceable - {#testCopyEmptyMethod}) + (TCopySequenceableSameContents - {#testSortBy. #integerCollection}) + TCopySequenceableWithOrWithoutSpecificElements + TCopyTest + (TCopySequenceableWithReplacement - {#testCopyReplaceAllWithManyOccurrence. #collectionWith2TimeSubcollection}) + TBeginsEndsWith + TRemoveTest + TSetArithmetic + TIncludesWithIdentityCheckTest + TStructuralEqualityTest + TOccurrencesTest + TSortTest',
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
	#category : #'Collections-Sequenceable-Tests-Base'
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

{ #category : #'tests - copying with replacement' }
LinkedListTest >> firstIndexesOf: aSubCollection in: collection [
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
LinkedListTest >> howMany: aSubCollection in: collection [
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
	self assert: list fourth equals: link4.

	list add: nil.
	self assert: list size equals: 5.
	self assert: list first equals: link1.
	self assert: list second equals: link2.
	self assert: list third equals: link3.
	self assert: list fourth equals: link4.
	self assert: list fifth equals: nil
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

{ #category : #'tests - iterating' }
LinkedListTest >> testAllSatisfyEmpty [

	self assert: ( self empty allSatisfy: [:each | false]).
]

{ #category : #'tests - iterating' }
LinkedListTest >> testAnySastify [

	| element |
	" when all elements satisty the condition, should return true :"
	self assert: ( self collectionWithoutNilElements anySatisfy: [:each | each notNil ]).
	
	" when only one element satisfy the condition, should return true :"
	element := self collectionWithoutNilElements anyOne.
	self assert: ( self collectionWithoutNilElements  anySatisfy: [:each | (each = element)  ]   ).
	
	" when all elements don't satisty the condition, should return false :"
	self deny: ( self collectionWithoutNilElements anySatisfy: [:each | (each notNil) not ]).
]

{ #category : #'tests - as string comma delimiter sequenceable' }
LinkedListTest >> testAsCommaStringEmpty [

	self assert: self empty asCommaString = ''.
	self assert: self empty asCommaStringAnd = ''
]

{ #category : #'tests - comma and delimiter' }
LinkedListTest >> testAsCommaStringOne [
	
	"self assert: self oneItemCol asCommaString = '1'.
	self assert: self oneItemCol asCommaStringAnd = '1'."

	self assert: self nonEmpty1Element asCommaString = (self nonEmpty1Element first asString).
	self assert: self nonEmpty1Element asCommaStringAnd = (self nonEmpty1Element first asString)
]

{ #category : #'tests - converting' }
LinkedListTest >> testAsIdentitySet [
	"Test with a collection without equal elements :"

	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: IdentitySet
]

{ #category : #'tests - converting' }
LinkedListTest >> testAsSet [
 	"Test with a collection without equal elements"
	
	self assertSameContents: self collectionWithoutEqualElements whenConvertedTo: Set
]

{ #category : #'tests - comma and delimiter' }
LinkedListTest >> testAsStringOnDelimiterLastMore [

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
LinkedListTest >> testAsStringOnDelimiterLastOne [

	| delim oneItemStream result |	
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]

{ #category : #'tests - comma and delimiter' }
LinkedListTest >> testAsStringOnDelimiterOne [

	| delim oneItemStream result |
	delim := ', '.
	result := ''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
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

{ #category : #'tests - iterating' }
LinkedListTest >> testBasicCollect [

	| res index |
	index := 0.
	res := self collectionWithoutNilElements collect: [ :each | 
		index := index + 1.
		each ].
	
	res do: [ :each | 
		self assert: (self collectionWithoutNilElements occurrencesOf: each) = (res occurrencesOf: each)].
	self assert: index equals: self collectionWithoutNilElements size.
]

{ #category : #'tests - iterating' }
LinkedListTest >> testBasicCollectThenDo [

	| result index |
	index := 0.
	result := self collectionWithoutNilElements 
		collect: [ :each | nil ]
		thenDo: [ :each | 
			self assert: each isNil.
			index := index + 1]	.
			
	self assert: result equals: self collectionWithoutNilElements.
	self assert: index equals: self collectionWithoutNilElements size.
]

{ #category : #'tests - copy' }
LinkedListTest >> testCopyNonEmptyWithout [
	
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
LinkedListTest >> testCopyReplaceAllWith1Occurrence [
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
LinkedListTest >> testCopyReplaceFromToWith [
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
LinkedListTest >> testCopyReplaceFromToWithInsertion [
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
LinkedListTest >> testCopyUpTo [

	| result index collection |
	collection := self collectionWithoutEqualElements.
	index:= self indexInForCollectionWithoutDuplicates.
	result := collection   copyUpTo: (collection  at:index).
	
	"Verify content"
	result withIndexDo: [:el :i| self assert: (collection at:i) equals: (result at:i)].
	
	"Verify size"
	self assert: result size equals: (index-1)
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

{ #category : #'tests - iterating' }
LinkedListTest >> testDetect [

	| res element |
	element := self collectionWithoutNilElements anyOne .
	 
	res := self collectionWithoutNilElements  detect: [:each | each = element].
	self assert: (res  = element).
]

{ #category : #'tests - iterating' }
LinkedListTest >> testDetectIfNone [

	| res element |
	res := self collectionWithoutNilElements  detect: [:each | each notNil not] ifNone: [100].
	self assert: res  = 100.
	
	element := self collectionWithoutNilElements anyOne.
	res := self collectionWithoutNilElements  detect: [:each | each = element] ifNone: [100].
	self assert: res  = element.
]

{ #category : #'tests - iterate on sequenced reable collections' }
LinkedListTest >> testDetectSequenced [
	"Testing that detect keep the first element returning true for sequenceable collections"

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element  detect: [:each | each notNil ].
	self assert: result equals: element
]

{ #category : #'tests - iterating' }
LinkedListTest >> testDo2 [
	"dc: Bad test, it assumes that a new instance of #speciesClass allows addition with #add:. This is not the case of Interval for which species is Array."
	"res := self speciesClass new.  
	self collection do: [:each | res add: each class].
	self assert: res = self result. "
	| collection cptElementsViewed cptElementsIn |
	collection := self collectionWithoutNilElements.
	cptElementsViewed := 0.
	cptElementsIn := OrderedCollection new.
	collection do: 
		[ :each | 
		cptElementsViewed := cptElementsViewed + 1.
		" #do doesn't iterate with the same objects than those in the collection for FloatArray( I don' t know why ) . That's why I use #includes: and not #identityIncludes:  '"
		(collection includes: each) ifTrue: [
			" the collection used doesn't include equal elements. Therefore each element viewed should not have been viewed before "
			( cptElementsIn includes: each ) ifFalse: [ cptElementsIn add: each ] .
			]. 
		].
	self assert: cptElementsViewed = collection size.
	self assert: cptElementsIn size  = collection size.
]

{ #category : #'tests - iterating' }
LinkedListTest >> testDoWithout [
	
	| res element collection |
	collection := self collectionWithoutNilElements .	
	res := OrderedCollection new.  
	element := self collectionWithoutNilElements anyOne .
	collection  do: [:each | res add: each] without: element  .
	" verifying result :"
	self assert: res size = (collection  size - (collection  occurrencesOf: element)).
	res do: [:each | self assert: (collection occurrencesOf: each) = ( res occurrencesOf: each ) ].
]

{ #category : #'tests - equality' }
LinkedListTest >> testEqualSignOfIdenticalCollectionObjects [
	
	self assert: self empty equals: self empty. 
	self assert: self nonEmpty equals: self nonEmpty
]

{ #category : #'tests - iterate on sequenced reable collections' }
LinkedListTest >> testFindFirst [

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element findFirst: [:each | each = element].
	
	self assert: result equals: 1
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

{ #category : #'tests - includes' }
LinkedListTest >> testIdentityIncludesNonSpecificComportement [
	"Test the same comportement than #includes:"
	
	| collection |	
	collection := self nonEmpty.
	
	self deny: (collection identityIncludes: self elementNotIn).
	self assert: (collection identityIncludes: collection anyOne)
]

{ #category : #'tests - set arithmetic' }
LinkedListTest >> testIntersectionItself [
	
	| result |
	result := self collectionWithoutEqualElements intersection: self collectionWithoutEqualElements.
	self assert: result size equals: self collectionWithoutEqualElements size.
	self collectionWithoutEqualElements do: [ :each|
		self assert: (result includes: each) ].
]

{ #category : #'tests - sorting' }
LinkedListTest >> testIsSortedBy [
	self assert: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a<b]).
	self deny: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a>b]).
]

{ #category : #'tests - iterate on sequenced reable collections' }
LinkedListTest >> testKeysAndValuesDo [
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

{ #category : #'tests - iterating' }
LinkedListTest >> testNoneSatisfyEmpty [

	self assert: ( self empty noneSatisfy: [:each | false]).
]

{ #category : #'tests - iterating' }
LinkedListTest >> testReject [

	| res element |
	res := self collectionWithoutNilElements reject: [:each | each notNil not].
	self assert: res size = self collectionWithoutNilElements size.
	
	element := self collectionWithoutNilElements anyOne.
	res := self collectionWithoutNilElements  reject: [:each | each = element].
	self assert: res size = (self collectionWithoutNilElements size - 1).
]

{ #category : #'tests - iterating' }
LinkedListTest >> testRejectEmpty [

	| res |
	res := self empty reject: [:each | each odd].
	self assert: res size = self empty size
]

{ #category : #'tests - iterating' }
LinkedListTest >> testRejectNoReject [

	| res |
	res := self collectionWithoutNilElements  reject: [:each | each notNil not].
	self assert: res size = self collectionWithoutNilElements size.
]

{ #category : #'tests - iterating' }
LinkedListTest >> testRejectThenCollect [

	| result index selectIndex pivot |
	
	index := 0.
	selectIndex := 0.
	pivot := self collectionWithoutNilElements anyOne.
	
	result := self collectionWithoutNilElements 
		reject: [ :each | 
			selectIndex := selectIndex + 1.
			"reject the first element"
			selectIndex = 1 ]
		thenCollect: [ :each | 
			self assert: each notNil.
			index := index + 1.
			pivot ].
			
	self assert: result ~= self collectionWithoutNilElements.
	self assert: selectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
	
	self assert: (self collectionWithoutNilElements occurrencesOf: pivot) equals: 1.
	"should be > 1 for standard collection and = 1 for those that do not allow exact duplicates"
	self assert: (result occurrencesOf: pivot) >= 1.
]

{ #category : #'tests - iterating' }
LinkedListTest >> testRejectThenDo [

	| result index rejectIndex |
	
	index := 0.
	rejectIndex := 0.
	
	result := self collectionWithoutNilElements 
		reject: [ :each | 
			rejectIndex := rejectIndex + 1.
			"reject the first element"
			rejectIndex = 1 ]
		thenDo: [ :each | 
			self assert: each notNil.
			index := index + 1]	.
			
	self assert: result equals: self collectionWithoutNilElements.
	self assert: rejectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
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

{ #category : #'tests - iterate on sequenced reable collections' }
LinkedListTest >> testReverseWithDo [
	
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

{ #category : #'tests - iterating' }
LinkedListTest >> testSelect [

	| result element |
	result := self collectionWithoutNilElements select: [ :each | each notNil].
	self assert: result size equals: self collectionWithoutNilElements size.
	
	element := self collectionWithoutNilElements anyOne.
	result := self collectionWithoutNilElements select: [ :each | (each = element) not].
	self assert: result size equals: (self collectionWithoutNilElements size - 1).
]

{ #category : #'tests - iterating' }
LinkedListTest >> testSelectThenCollect [

	| result index selectIndex pivot |
	
	index := 0.
	selectIndex := 0.
	pivot := self collectionWithoutNilElements anyOne.
	
	result := self collectionWithoutNilElements 
		select: [ :each | 
			selectIndex := selectIndex + 1.
			"reject the first element"
			selectIndex > 1 ]
		thenCollect: [ :each | 
			self assert: each notNil.
			index := index + 1.
			pivot ].
			
	self assert: result ~= self collectionWithoutNilElements.
	self assert: selectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
	
	self assert: (self collectionWithoutNilElements occurrencesOf: pivot) equals: 1.
	"should be > 1 for standard collection and = 1 for those that do not allow exact duplicates"
	self assert: (result occurrencesOf: pivot) >= 1.
]

{ #category : #'tests - iterating' }
LinkedListTest >> testSelectThenDo [

	| result index selectIndex |
	
	index := 0.
	selectIndex := 0.
	
	result := self collectionWithoutNilElements 
		select: [ :each | 
			selectIndex := selectIndex + 1.
			"reject the first element"
			selectIndex > 1 ]
		thenDo: [ :each | 
			self assert: each notNil.
			index := index + 1]	.
			
	self assert: result equals: self collectionWithoutNilElements.
	self assert: selectIndex equals: self collectionWithoutNilElements size.
	self assert: index equals: self collectionWithoutNilElements size - 1.
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

{ #category : #'tests - adding' }
LinkedListTest >> testTAddIfNotPresentWithNewElement [

	| added oldSize collection elem |
	collection := self otherCollection.
	oldSize := collection size.
	elem := self element.
	self deny: (collection includes: elem ).
	
	added := collection  addIfNotPresent: elem .
	self assert: added == elem . "test for identiy because #add: has not reason to copy its parameter."
	self assert: collection size equals: (oldSize + 1).
]

{ #category : #'tests - iterate on sequenced reable collections' }
LinkedListTest >> testWithDo [
	
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
LinkedListTest >> testWithIndexCollect [
	
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
LinkedListTest >> testWithIndexDo [

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

{ #category : #'tests - begins ends with' }
LinkedListTest >> testsBeginsWithEmpty [
	
	self deny: (self nonEmpty beginsWith:(self empty)).
	self deny: (self empty beginsWith:(self nonEmpty))
]

{ #category : #'tests - begins ends with' }
LinkedListTest >> testsEndsWithEmpty [
	
	self deny: (self nonEmpty endsWith: self empty).
	self deny: (self empty endsWith: self nonEmpty)
]

{ #category : #requirements }
LinkedListTest >> unsortedCollection [
"Return a collection that is not yet sorted"
	^ unsortedCollection
]
