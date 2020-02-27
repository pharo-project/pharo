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
	#category : #'Collections-Tests-Strings'
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
	| result |
	
	^result := { 
		[Symbol readFromString: '#''abc'''] -> #abc.
		[Symbol readFromString: '#ab-C'] -> #ab.
	 	[Symbol readFromString: '#abC'] -> #abC. 
		[Symbol readFromString: '#ab@c'] -> #ab}.
]

{ #category : #requirements }
SymbolTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	^ nonEmptySymbol5ElementsNoDuplicates 
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

{ #category : #tests }
SymbolTest >> testAsMutator [
	self assert: #x asMutator equals: #x:.
	"return receiver if it is already a mutator"
	self assert: #x: asMutator equals: #x:.
	self assert: #x asMutator isSymbol
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
SymbolTest >> testCopyNonEmptyWithoutAllNotIncluded [
	
]

{ #category : #'tests - copy' }
SymbolTest >> testCopyNotSame [
	"There is only one symbol, the copy is the same"

	| copy |
	copy := self nonEmpty copy.
	self assert: copy identicalTo: self nonEmpty
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

{ #category : #tests }
SymbolTest >> testIsLiteralSymbol [

	self assert: ('*+-/\~=<>&@%,|' allSatisfy: [:char | (Symbol with: char) isLiteralSymbol])
		description: 'single letter binary symbols can be printed without string quotes'.
		
	self assert: (#('x' 'x:' 'x:y:' 'from:to:by:' 'yourself') allSatisfy: [:str | str asSymbol isLiteralSymbol])
		description: 'valid ascii selector symbols can be printed without string quotes'.
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

{ #category : #tests }
SymbolTest >> testReadFrom [
	self expectedBehavior do: [ :association | self assert: association key value identicalTo: association value ]
]

{ #category : #tests }
SymbolTest >> testUncapitalized [
	| uc lc empty |
	uc := #MElViN.
	lc := #mElViN.
	empty := #' '.
	self assert: uc uncapitalized equals: lc.
	self assert: lc uncapitalized equals: lc
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
