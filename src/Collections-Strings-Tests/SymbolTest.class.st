"
This is the unit test for the class Symbol. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org/)
	- the sunit class category
"
Class {
	#name : #SymbolTest,
	#superclass : #CollectionRootTest,
	#traits : 'TSortTest - {#testSort. #testSortUsingSortBlock} + TIncludesTest + (TCloneTest - {#testCopyCreatesNewObject}) + TCopyPreservingIdentityTest + TCopyTest + (TSetArithmetic - {#testDifferenceWithNonNullIntersection}) + TIterateSequencedReadableTest + TSequencedConcatenationTest + TPrintOnSequencedTest + TAsStringCommaAndDelimiterSequenceableTest + TIndexAccess + (TIndexAccessForMultipliness - {#testIdentityIndexOfIAbsentDuplicate. #testIdentityIndexOfDuplicate. #collectionWithNonIdentitySameAtEndAndBegining}) + TSequencedElementAccessTest + TSubCollectionAccess + (TCopySequenceableSameContents - {#testShuffled}) + (TCopyPartOfSequenceable - {#testCopyEmptyMethod. #integerCollectionWithoutEqualElements}) + TCopyPartOfSequenceableForMultipliness + TCopySequenceableWithReplacement + TBeginsEndsWith + TConvertAsSortedTest + (TConvertTest - {#testAsByteArray. #integerCollectionWithoutEqualElements}) + TCopySequenceableWithOrWithoutSpecificElements + TConvertAsSetForMultiplinessTest + TSequencedStructuralEqualityTest + TOccurrencesForMultiplinessTest',
	#classTraits : 'TSortTest classTrait + TIncludesTest classTrait + TCloneTest classTrait + TCopyPreservingIdentityTest classTrait + TCopyTest classTrait + TSetArithmetic classTrait + TIterateSequencedReadableTest classTrait + TSequencedConcatenationTest classTrait + TPrintOnSequencedTest classTrait + TAsStringCommaAndDelimiterSequenceableTest classTrait + TIndexAccess classTrait + TIndexAccessForMultipliness classTrait + TSequencedElementAccessTest classTrait + TSubCollectionAccess classTrait + TCopySequenceableSameContents classTrait + TCopyPartOfSequenceable classTrait + TCopyPartOfSequenceableForMultipliness classTrait + TCopySequenceableWithReplacement classTrait + TBeginsEndsWith classTrait + TConvertAsSortedTest classTrait + TConvertTest classTrait + TCopySequenceableWithOrWithoutSpecificElements classTrait + TConvertAsSetForMultiplinessTest classTrait + TSequencedStructuralEqualityTest classTrait + TOccurrencesForMultiplinessTest classTrait',
	#instVars : [
		'emptySymbol',
		'nonEmptySymbol5ElementsNoDuplicates',
		'notIn',
		'collectionNotIncluded',
		'collectionSize4',
		'collection1Element',
		'sameAtEndAndBegining',
		'with2timeSubcollection',
		'subcollection3ElementsSorted'
	],
	#category : #'Collections-Strings-Tests-Base'
}

{ #category : #requirements }
SymbolTest >> anotherElementNotIn [
	^ notIn
]

{ #category : #requirements }
SymbolTest >> anotherElementOrAssociationIn [
	" return an element (or an association for Dictionary ) present  in 'collection' "
	^ self collection anyOne
]

{ #category : #requirements }
SymbolTest >> anotherElementOrAssociationNotIn [
	" return an element (or an association for Dictionary )not present  in 'collection' "
	^ notIn
]

{ #category : #coverage }
SymbolTest >> classToBeTested [
	
	^ Symbol
]

{ #category : #requirements }
SymbolTest >> collection [

	^ nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #'tests - set arithmetic' }
SymbolTest >> collectionClass [
	"Return the class to be used to create instances of the class tested"
	
	^ Symbol
]

{ #category : #requirements }
SymbolTest >> collectionMoreThan1NoDuplicates [
	" return a collection of size > 1 without equal elements"
	^ nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #requirements }
SymbolTest >> collectionNotIncluded [
" return a collection for wich each element is not included in 'nonEmpty' "
	^ collectionNotIncluded
]

{ #category : #requirements }
SymbolTest >> collectionWith1TimeSubcollection [
" return a collection including 'oldSubCollection'  only one time "
	^ nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #requirements }
SymbolTest >> collectionWith2TimeSubcollection [
" return a collection including 'oldSubCollection'  two or many time "
	^ with2timeSubcollection
]

{ #category : #requirements }
SymbolTest >> collectionWithElementsToRemove [
" return a collection of elements included in 'nonEmpty'  "
	^ subcollection3ElementsSorted
]

{ #category : #requirements }
SymbolTest >> collectionWithEqualElements [
" return a collecition including atLeast two elements equal"

^ sameAtEndAndBegining .
]

{ #category : #requirements }
SymbolTest >> collectionWithSameAtEndAndBegining [
	" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	^ sameAtEndAndBegining
]

{ #category : #requirements }
SymbolTest >> collectionWithSortableElements [
	" return a collection only including elements that can be sorted (understanding '<' )"
	^ nonEmptySymbol5ElementsNoDuplicates .
]

{ #category : #requirements }
SymbolTest >> collectionWithoutEqualElements [

" return a collection not including equal elements "
	^ nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #requirements }
SymbolTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	^nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #requirements }
SymbolTest >> elementInForElementAccessing [
" return an element inculded in 'moreThan4Elements'"
	^ self moreThan4Elements anyOne
]

{ #category : #requirements }
SymbolTest >> elementInForIndexAccessing [
" return an element included in 'collectionMoreThan1NoDuplicates' "
	^ self collectionMoreThan1NoDuplicates anyOne.
]

{ #category : #requirements }
SymbolTest >> elementNotIn [
"return an element not included in 'nonEmpty' "

	^ notIn
]

{ #category : #requirements }
SymbolTest >> elementNotInForElementAccessing [
" return an element not included in 'moreThan4Elements' "
	^ notIn
]

{ #category : #requirements }
SymbolTest >> elementNotInForIndexAccessing [
" return an element not included in 'collectionMoreThan1NoDuplicates' "
	^ notIn
]

{ #category : #requirements }
SymbolTest >> elementNotInForOccurrences [
	^ notIn
]

{ #category : #'tests - copying' }
SymbolTest >> elementToAdd [
	^ $u
]

{ #category : #requirements }
SymbolTest >> elementTwiceInForOccurrences [
" return an element included exactly two time in # collectionWithEqualElements"
^ sameAtEndAndBegining first
]

{ #category : #requirements }
SymbolTest >> empty [
	^ emptySymbol
]

{ #category : #accessing }
SymbolTest >> expectedBehavior [
	^ { 
		  ([ Symbol readFromString: '#''abc''' ] -> #abc).
		  ([ Symbol readFromString: '#ab-C' ] -> #ab).
		  ([ Symbol readFromString: '#abC' ] -> #abC).
		  ([ Symbol readFromString: '#ab@c' ] -> #ab) }
]

{ #category : #requirements }
SymbolTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	^ nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #'tests - copying with replacement' }
SymbolTest >> firstIndexesOf: aSubCollection in: collection [
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
SymbolTest >> howMany: aSubCollection in: collection [
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
SymbolTest >> indexInForCollectionWithoutDuplicates [
" return an index between 'collectionWithoutEqualsElements'  bounds"
	^ 2
]

{ #category : #requirements }
SymbolTest >> indexInNonEmpty [
	"Return an index between bounds of 'nonEmpty'"

	^ 2
]

{ #category : #requirements }
SymbolTest >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	^ nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #requirements }
SymbolTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	^ nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #requirements }
SymbolTest >> nonEmpty [
	^ nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #requirements }
SymbolTest >> nonEmpty1Element [
" return a collection of size 1 including one element"
	^collection1Element
]

{ #category : #requirements }
SymbolTest >> nonEmptyMoreThan1Element [
" return a collection that don't includes equal elements'" 
	^nonEmptySymbol5ElementsNoDuplicates
]

{ #category : #requirements }
SymbolTest >> oldSubCollection [
" return a subCollection included in collectionWith1TimeSubcollection .
ex :   subCollection := #( 2 3 4) and collectionWith1TimeSubcollection := #(1 2 3 4 5)"
	^ subcollection3ElementsSorted
]

{ #category : #requirements }
SymbolTest >> replacementCollection [
" return a collection that will be used to replace 'oldSubcollection' in ' collectionWith1TimeSubcollection'  " 
	^ collection1Element
]

{ #category : #requirements }
SymbolTest >> secondCollection [
" return a collection that will be the second part of the concatenation"
	^ collectionSize4
]

{ #category : #running }
SymbolTest >> setUp [
	super setUp.
	emptySymbol := #''.
	collectionSize4 := #abcd.
	collection1Element := #a.
	subcollection3ElementsSorted := #bcd.
	with2timeSubcollection := #abcdebcda.
	nonEmptySymbol5ElementsNoDuplicates := #abcde.
	sameAtEndAndBegining := #abcda.
	notIn := $z.
	collectionNotIncluded := #zz.
]

{ #category : #requirements }
SymbolTest >> sizeCollection [
	"Answers a collection whose #size is 4"
	^ collectionSize4
]

{ #category : #requirements }
SymbolTest >> sortedInAscendingOrderCollection [
	"Return a collection sorted in ascending order"
	^ collectionSize4
]

{ #category : #requirements }
SymbolTest >> subCollectionNotIn [
" return a collection for which at least one element is not included in 'moreThan4Elements' "
	^ collectionNotIncluded
]

{ #category : #'tests - as string comma delimiter sequenceable' }
SymbolTest >> testAsCommaStringEmpty [

	self assert: self empty asCommaString = ''.
	self assert: self empty asCommaStringAnd = ''
]

{ #category : #'tests - comma and delimiter' }
SymbolTest >> testAsCommaStringOne [
	
	"self assert: self oneItemCol asCommaString = '1'.
	self assert: self oneItemCol asCommaStringAnd = '1'."

	self assert: self nonEmpty1Element asCommaString = (self nonEmpty1Element first asString).
	self assert: self nonEmpty1Element asCommaStringAnd = (self nonEmpty1Element first asString)
]

{ #category : #'tests - converting' }
SymbolTest >> testAsIdentitySet [
	"Test with a collection without equal elements :"

	self 
		assertSameContents: self collectionWithoutEqualElements
		whenConvertedTo: IdentitySet
]

{ #category : #tests }
SymbolTest >> testAsMutator [
	self assert: #x asMutator equals: #x:.
	"return receiver if it is already a mutator"
	self assert: #x: asMutator equals: #x:.
	self assert: #x asMutator isSymbol
]

{ #category : #'tests - converting' }
SymbolTest >> testAsSet [
 	"Test with a collection without equal elements"
	
	self assertSameContents: self collectionWithoutEqualElements whenConvertedTo: Set
]

{ #category : #'tests - as sorted collection' }
SymbolTest >> testAsSortedCollectionWithSortBlock [

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
SymbolTest >> testAsStringOnDelimiterLastMore [

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
SymbolTest >> testAsStringOnDelimiterLastOne [

	| delim oneItemStream result |	
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]

{ #category : #'tests - comma and delimiter' }
SymbolTest >> testAsStringOnDelimiterOne [

	| delim oneItemStream result |
	delim := ', '.
	result := ''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)]
]

{ #category : #tests }
SymbolTest >> testCapitalized [
	| uc lc |
	uc := #MElViN.
	lc := #mElViN.
	self assert: lc capitalized equals: uc.
	self assert: uc capitalized equals: uc
]

{ #category : #tests }
SymbolTest >> testConcatenationIsSymbol [
	"Concatenating 2 symbols results in another symbol"

	self assert: (#foo , #bar) isSymbol.
	self assert: #foo , #bar identicalTo: #foobar.

	"Concatenating the empty Symbol does not change a Symbol"
	self assert: #foo , emptySymbol identicalTo: #foo.
	self assert: emptySymbol , #foo identicalTo: #foo.

	"Strings and Symbols can still be mixed, the receiver determines the result type"
	"Symbol receiver gives Symbol result"
	self assert: (#foo , 'bar') isSymbol.
	self assert: #foo , 'bar' identicalTo: #foobar.
	"String receiver gives String result"
	self assert: ('foo' , #bar) isString.
	self assert: 'foo' , #bar equals: 'foobar'.
	"Strings and Symbols still compare content-wise"
	self assert: 'foo' , #bar equals: #foobar.
	"But Strings and Symbols are not identical"
	self deny: 'foo' , #bar identicalTo: #foobar
]

{ #category : #'tests - copy' }
SymbolTest >> testCopyNonEmptyWithout [
	
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
SymbolTest >> testCopyNonEmptyWithoutAllNotIncluded [
]

{ #category : #'tests - copy' }
SymbolTest >> testCopyNotSame [
	"There is only one symbol, the copy is the same"

	| copy |
	copy := self nonEmpty copy.
	self assert: copy identicalTo: self nonEmpty
]

{ #category : #'tests - copying with replacement' }
SymbolTest >> testCopyReplaceAllWith1Occurrence [
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
SymbolTest >> testCopyReplaceFromToWith [
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
SymbolTest >> testCopyReplaceFromToWithInsertion [
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

{ #category : #'tests - copy' }
SymbolTest >> testCopyReturnsIdentity [
	
	| copy | 
	copy := self nonEmpty copy.
	self assert: self nonEmpty == copy.
]

{ #category : #'tests - copying part of sequenceable' }
SymbolTest >> testCopyUpTo [

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
SymbolTest >> testCopyUpToLastWithDuplicate [

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
SymbolTest >> testDetectSequenced [
	"Testing that detect keep the first element returning true for sequenceable collections"

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element  detect: [:each | each notNil ].
	self assert: result equals: element
]

{ #category : #tests }
SymbolTest >> testDisplayString [

	| actual |
	actual := #'some symbol' displayString.
	self assert: actual equals: 'some symbol'
]

{ #category : #'tests - testing' }
SymbolTest >> testEndsWithAColon [
	 
	self assert: #fred: endsWithAColon.

	self deny: #'fred' endsWithAColon.

	self deny: #fred endsWithAColon.
]

{ #category : #'tests - equality' }
SymbolTest >> testEqualSignIsTrueForEmptyButNonIdenticalCollections [
	
	self assert: self empty equals: self empty copy. 
	self assert: self empty copy equals: self empty.
	self assert: self empty copy equals: self empty copy
]

{ #category : #'tests - equality' }
SymbolTest >> testEqualSignOfIdenticalCollectionObjects [
	
	self assert: self empty equals: self empty. 
	self assert: self nonEmpty equals: self nonEmpty
]

{ #category : #'tests - iterate on sequenced reable collections' }
SymbolTest >> testFindFirst [

	| element result |
	element := self nonEmptyMoreThan1Element at: 1.
	result := self nonEmptyMoreThan1Element findFirst: [:each | each = element].
	
	self assert: result equals: 1
]

{ #category : #'tests - equality' }
SymbolTest >> testHasEqualElementsIsTrueForNonIdenticalButEqualCollections [
		
	self assert: (self empty hasEqualElements: self empty copy). 
	self assert: (self empty copy hasEqualElements: self empty).
	self assert: (self empty copy hasEqualElements: self empty copy).
		
	self assert: (self nonEmpty hasEqualElements: self nonEmpty copy). 
	self assert: (self nonEmpty copy hasEqualElements: self nonEmpty).
	self assert: (self nonEmpty copy hasEqualElements: self nonEmpty copy)
]

{ #category : #'tests - equality' }
SymbolTest >> testHasEqualElementsOfIdenticalCollectionObjects [
	
	self assert: (self empty hasEqualElements: self empty). 
	self assert: (self nonEmpty hasEqualElements: self nonEmpty)
]

{ #category : #'tests - includes' }
SymbolTest >> testIdentityIncludesNonSpecificComportement [
	"Test the same comportement than #includes:"
	
	| collection |	
	collection := self nonEmpty.
	
	self deny: (collection identityIncludes: self elementNotIn).
	self assert: (collection identityIncludes: collection anyOne)
]

{ #category : #'tests - set arithmetic' }
SymbolTest >> testIntersectionItself [
	
	| result |
	result := self collectionWithoutEqualElements intersection: self collectionWithoutEqualElements.
	self assert: result size equals: self collectionWithoutEqualElements size.
	self collectionWithoutEqualElements do: [ :each|
		self assert: (result includes: each) ].
]

{ #category : #tests }
SymbolTest >> testIsLiteralSymbol [

	self assert: ('*+-/\~=<>&@%,|' allSatisfy: [:char | (Symbol with: char) isLiteralSymbol])
		description: 'single letter binary symbols can be printed without string quotes'.
		
	self assert: (#('x' 'x:' 'x:y:' 'from:to:by:' 'yourself') allSatisfy: [:str | str asSymbol isLiteralSymbol])
		description: 'valid ascii selector symbols can be printed without string quotes'.
]

{ #category : #'tests - sorting' }
SymbolTest >> testIsSortedBy [
	self assert: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a<b]).
	self deny: (self sortedInAscendingOrderCollection isSortedBy: [:a :b | a>b]).
]

{ #category : #'tests - iterate on sequenced reable collections' }
SymbolTest >> testKeysAndValuesDo [
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

{ #category : #requirements }
SymbolTest >> testNew [
	
	self should: [Symbol new: 5 ] raise: self defaultTestError.
]

{ #category : #tests }
SymbolTest >> testNumArgs2 [
	"TODO: need to be extended to support shrinking and for selectors like #+ "

	self assert: (#test numArgs: 0) equals: #test.
	self assert: (#test numArgs: 1) equals: #test:.
	self assert: (#test numArgs: 2) equals: #test:with:.
	self assert: (#test numArgs: 3) equals: #test:with:with:.


	self assert: (#test: numArgs: 0) equals: #test:.
	self assert: (#test: numArgs: 1) equals: #test:.
	self assert: (#test: numArgs: 2) equals: #test:with:.
	self assert: (#test: numArgs: 3) equals: #test:with:with:.

	self assert: (#test:with: numArgs: 0) equals: #test:with:.
	self assert: (#test:with: numArgs: 1) equals: #test:with:.
	self assert: (#test:with: numArgs: 2) equals: #test:with:.
	self assert: (#test:with: numArgs: 3) equals: #test:with:with:.
	self assert: (#test:with: numArgs: 4) equals: #test:with:with:with:.

	self assert: (#test:with:with: numArgs: 0) equals: #test:with:with:.
	self assert: (#test:with:with: numArgs: 1) equals: #test:with:with:.
	self assert: (#test:with:with: numArgs: 2) equals: #test:with:with:.
	self assert: (#test:with:with: numArgs: 3) equals: #test:with:with:.
	self assert: (#test:with:with: numArgs: 4) equals: #test:with:with:with:
]

{ #category : #tests }
SymbolTest >> testNumArgsBinary [
	self assert: '-' numArgs equals: 1.
	self assert: '++' numArgs equals: 1.
	self assert: '*&@' numArgs equals: 1.

	self assert: ' ++' numArgs equals: -1.
	self assert: '+ +' numArgs equals: -1.
	self assert: '++ ' numArgs equals: -1.

	self assert: '_++' numArgs equals: -1.
	self assert: '+_+' numArgs equals: -1.
	self assert: '++_' numArgs equals: -1.

	self assert: '1' numArgs equals: -1.
	self assert: '*a' numArgs equals: -1
]

{ #category : #tests }
SymbolTest >> testNumArgsKeyword1 [
	self assert: 'name:' numArgs equals: 1.

	self assert: 'name_:' numArgs equals: 1.
	self assert: '_name:' numArgs equals: 1.
	self assert: 'na_me:' numArgs equals: 1.

	self assert: 'name: ' numArgs equals: -1.
	self assert: ' name:' numArgs equals: -1.
	self assert: 'na me:' numArgs equals: -1.

	self assert: 'name+:' numArgs equals: -1.
	self assert: '+name:' numArgs equals: -1.
	self assert: 'na+me:' numArgs equals: -1.

	self assert: 'name123:' numArgs equals: 1.
	self assert: 'na123me:' numArgs equals: 1.
	self assert: '123name:' numArgs equals: -1
]

{ #category : #tests }
SymbolTest >> testNumArgsKeyword2 [
	self assert: 'name:with:' numArgs equals: 2.

	self assert: '_name:with:' numArgs equals: 2.
	self assert: 'na_me:with:' numArgs equals: 2.
	self assert: 'name_:with:' numArgs equals: 2.

	self assert: 'name:_with:' numArgs equals: 2.
	self assert: 'name:wi_th:' numArgs equals: 2.
	self assert: 'name:with_:' numArgs equals: 2.

	self assert: 'name:with: ' numArgs equals: -1.
	self assert: 'name: with:' numArgs equals: -1.
	self assert: ' name:with:' numArgs equals: -1.

	self assert: 'name+:with:' numArgs equals: -1.
	self assert: '+name:with:' numArgs equals: -1.
	self assert: 'na+me:with:' numArgs equals: -1.

	self assert: '123name:with:' numArgs equals: -1.
	self assert: 'name123:with:' numArgs equals: 2.
	self assert: 'name:123with:' numArgs equals: -1.
	self assert: 'name:with123:' numArgs equals: 2
]

{ #category : #tests }
SymbolTest >> testNumArgsUnary [
	self assert: 'name' numArgs equals: 0.

	self assert: 'name_' numArgs equals: 0.
	self assert: '_name' numArgs equals: 0.
	self assert: 'na_me' numArgs equals: 0.

	self assert: 'name ' numArgs equals: -1.
	self assert: ' name' numArgs equals: -1.
	self assert: 'na me' numArgs equals: -1.

	self assert: 'name+' numArgs equals: -1.
	self assert: '+name' numArgs equals: -1.
	self assert: 'na+me' numArgs equals: -1.

	self assert: 'name123' numArgs equals: 0.
	self assert: 'na123me' numArgs equals: 0.
	self assert: '123name' numArgs equals: -1
]

{ #category : #'tests - occurrencesOf for multipliness' }
SymbolTest >> testOccurrencesOfForMultipliness [

	| collection elem |
	collection := self collectionWithEqualElements.
	elem := self elementTwiceInForOccurrences.

	self assert: (collection occurrencesOf: elem ) equals: 2.
]

{ #category : #tests }
SymbolTest >> testReadFrom [
	self expectedBehavior do: [ :association | self assert: association key value identicalTo: association value ]
]

{ #category : #'tests - iterate on sequenced reable collections' }
SymbolTest >> testReverseWithDo [
	
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
SymbolTest >> testUncapitalized [
	| uc lc |
	uc := #MElViN.
	lc := #mElViN.
	self assert: uc uncapitalized equals: lc.
	self assert: lc uncapitalized equals: lc
]

{ #category : #'tests - iterate on sequenced reable collections' }
SymbolTest >> testWithDo [
	
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
SymbolTest >> testWithIndexCollect [
	
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
SymbolTest >> testWithIndexDo [

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
SymbolTest >> testsBeginsWithEmpty [
	
	self deny: (self nonEmpty beginsWith:(self empty)).
	self deny: (self empty beginsWith:(self nonEmpty))
]

{ #category : #'tests - begins ends with' }
SymbolTest >> testsEndsWithEmpty [
	
	self deny: (self nonEmpty endsWith: self empty).
	self deny: (self empty endsWith: self nonEmpty)
]

{ #category : #requirements }
SymbolTest >> unsortedCollection [
" retur a collection that is not yat sorted"
	^ with2timeSubcollection
]

{ #category : #requirements }
SymbolTest >> withEqualElements [
	" return a collection  including equal elements (classic equality)"
	^ sameAtEndAndBegining
]
