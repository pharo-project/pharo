Class {
	#name : 'Float64ArrayTest',
	#superclass : 'CollectionRootTest',
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
	#category : 'Collections-Native-Tests-Base',
	#package : 'Collections-Native-Tests',
	#tag : 'Base'
}

{ #category : 'requirements' }
Float64ArrayTest >> aValue [
" return a value to put into nonEmpty"
	^ elementNotIn
]

{ #category : 'requirements' }
Float64ArrayTest >> anIndex [
" return an index in nonEmpty bounds"
	^ 2
]

{ #category : 'requirements' }
Float64ArrayTest >> anotherElementNotIn [
" return an element different of 'elementNotIn'  not included in 'nonEmpty' "
	^ elementNotIn
]

{ #category : 'requirements' }
Float64ArrayTest >> anotherElementOrAssociationIn [
	" return an element (or an association for Dictionary ) present  in 'collection' "
	^ self collection anyOne
]

{ #category : 'requirements' }
Float64ArrayTest >> anotherElementOrAssociationNotIn [
	" return an element (or an association for Dictionary )not present  in 'collection' "
	^ elementNotIn
]

{ #category : 'requirements' }
Float64ArrayTest >> anotherValue [
" return a value ( not eual to 'aValue' ) to put into nonEmpty "
	^ elementInNonEmpty
]

{ #category : 'coverage' }
Float64ArrayTest >> classToBeTested [

	^ Float64Array
]

{ #category : 'requirements' }
Float64ArrayTest >> collection [

	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionClass [
	"Return the class to be used to create instances of the class tested"

	^ Float64Array
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionMoreThan1NoDuplicates [
	" return a collection of size > 1 without equal elements"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionMoreThan5Elements [
" return a collection including at least 5 elements"

	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionNotIncluded [
" return a collection for wich each element is not included in 'nonEmpty' "
	^ collectionNotIncluded
		ifNil: [ collectionNotIncluded := (Float64Array new: 2) at:1 put: elementNotIn ; at: 2 put: elementNotIn  ; yourself ]
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWith1TimeSubcollection [
" return a collection including 'oldSubCollection'  only one time "
	^ collectionWith1TimeSubcollection ifNil: [ collectionWith1TimeSubcollection := collectionWithSameAtEndAndBegining  , self oldSubCollection , collectionWithSameAtEndAndBegining  ]
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWith2TimeSubcollection [
" return a collection including 'oldSubCollection'  two or many time "
	^ collectionWith2TimeSubcollection ifNil: [ collectionWith2TimeSubcollection := self collectionWith1TimeSubcollection, self oldSubCollection  ]
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWithCopyNonIdentical [
	" return a collection that include elements for which 'copy' return a different object (this is not the case of SmallInteger)"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWithElementsToRemove [
" return a collection of elements included in 'nonEmpty'  "
	^ nonEmptySubcollection
	ifNil: [ nonEmptySubcollection := (Float64Array new:2 ) at:1 put: self nonEmpty first ; at:2 put: self nonEmpty last ; yourself ]
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWithNonIdentitySameAtEndAndBegining [
	" return a collection with elements at end and begining equals only with classic equality (they are not the same object).
(others elements of the collection are not equal to those elements)"
	^ collectionWithSameAtEndAndBegining
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWithSameAtEndAndBegining [
" return a collection with elements at end and begining equals .
(others elements of the collection are not equal to those elements)"
	^ collectionWithSameAtEndAndBegining
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWithSortableElements [
" return a collection elements that can be sorte ( understanding message ' < '  or ' > ')"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWithoutEqualElements [

" return a collection not including equal elements "
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> collectionWithoutNilElements [
" return a collection that doesn't includes a nil element  and that doesn't includes equal elements'"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> elementInForElementAccessing [
" return an element inculded in 'moreThan4Elements'"
	^ elementInNonEmpty
]

{ #category : 'requirements' }
Float64ArrayTest >> elementInForIndexAccessing [
" return an element included in 'collectionMoreThan1NoDuplicates' "
	^ elementInNonEmpty
]

{ #category : 'requirements' }
Float64ArrayTest >> elementNotIn [
"return an element not included in 'nonEmpty' "

	^ elementNotIn
]

{ #category : 'requirements' }
Float64ArrayTest >> elementNotInForElementAccessing [
" return an element not included in 'moreThan4Elements' "
	^ elementNotIn
]

{ #category : 'requirements' }
Float64ArrayTest >> elementNotInForIndexAccessing [
" return an element not included in 'collectionMoreThan1NoDuplicates' "
	^ elementNotIn
]

{ #category : 'requirements' }
Float64ArrayTest >> elementToAdd [
" return an element of type 'nonEmpy' elements'type'  not  yet included in nonEmpty"
	^ elementNotIn
]

{ #category : 'requirements' }
Float64ArrayTest >> elementsCopyNonIdenticalWithoutEqualElements [
	" return a collection that does niot incllude equal elements ( classic equality )
	all elements included are elements for which copy is not identical to the element  "
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> empty [
	^ empty
]

{ #category : 'requirements' }
Float64ArrayTest >> firstCollection [
" return a collection that will be the first part of the concatenation"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> firstIndex [
" return an index between 'nonEmpty' bounds that is < to 'second index' "
	^2
]

{ #category : 'requirements' }
Float64ArrayTest >> indexArray [
" return a Collection including indexes between bounds of 'nonEmpty' "

	^ { 1. 4. 3.}
]

{ #category : 'requirements' }
Float64ArrayTest >> indexInForCollectionWithoutDuplicates [
" return an index between 'collectionWithoutEqualsElements'  bounds"
	^ 2
]

{ #category : 'requirements' }
Float64ArrayTest >> indexInNonEmpty [
	"Return an index between bounds of 'nonEmpty'"

	^ 3
]

{ #category : 'requirements' }
Float64ArrayTest >> moreThan3Elements [
	" return a collection including atLeast 3 elements"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> moreThan4Elements [

" return a collection including at leat 4 elements"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> newElement [
"return an element that will be put in the collection in place of another"
	^ elementNotIn
]

{ #category : 'requirements' }
Float64ArrayTest >> nonEmpty [

	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> nonEmpty1Element [
" return a collection of size 1 including one element"
	^ nonEmpty1Element
]

{ #category : 'requirements' }
Float64ArrayTest >> nonEmptyMoreThan1Element [
" return a collection that doesn't includes equal elements' and doesn't include nil elements'"
	^nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> oldSubCollection [
" return a subCollection included in collectionWith1TimeSubcollection .
ex :   subCollection := #( 2 3 4) and collectionWith1TimeSubcollection := #(1 2 3 4 5)"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> replacementCollection [
" return a collection that will be used to replace 'oldSubcollection' in ' collectionWith1TimeSubcollection'  "
	^ collectionWithSameAtEndAndBegining
]

{ #category : 'requirements' }
Float64ArrayTest >> replacementCollectionSameSize [
" return a collection of size (secondIndex - firstIndex + 1)"
	^replacementCollectionSameSize
		ifNil: [ 	replacementCollectionSameSize := Float32Array new: (self secondIndex  - self firstIndex  + 1).
				 1 to: replacementCollectionSameSize size do:
					[ :i | replacementCollectionSameSize at:i put: elementInNonEmpty ].
				replacementCollectionSameSize.
				 ]
]

{ #category : 'requirements' }
Float64ArrayTest >> secondCollection [
" return a collection that will be the second part of the concatenation"
	^ collectionWithEqualElements
]

{ #category : 'requirements' }
Float64ArrayTest >> secondIndex [
" return an index between 'nonEmpty' bounds that is > to 'first index' "
	^self firstIndex +1
]

{ #category : 'running' }
Float64ArrayTest >> setUp [
	super setUp.
	empty := Float64Array new.
	elementInNonEmpty := 7.0.
	nonEmpty5ElementsNoDuplicate := (Float64Array new: 5)
		at: 1 put: 1.5;
		at: 2 put: 2.5;
		at: 3 put: elementInNonEmpty;
		at: 4 put: 4.5;
		at: 5 put: 5.5;
		yourself.
	elementNotIn := 999.0.
	elementTwiceIn := 2.3.
	collectionWithEqualElements := (Float64Array new: 3)
		at: 1 put: 2.0;
		at: 2 put: 2.0;
		at: 3 put: 3.5;
		yourself.
	nonEmpty1Element := (Float64Array new: 1)
		at: 1 put: 1.2;
		yourself.
	collectionWithSameAtEndAndBegining := (Float64Array new: 3)
		at: 1 put: 2.0;
		at: 2 put: 1.0;
		at: 3 put: 2.0 copy;
		yourself
]

{ #category : 'requirements' }
Float64ArrayTest >> sizeCollection [
	"Answers a collection not empty"
	^ nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> sortedInAscendingOrderCollection [
	"Return a collection sorted in ascending order"
	^ sortedCollection ifNil: [ sortedCollection := ( Float64Array new: 3)at: 1 put: 1.0 ; at: 2 put: 2.0 ; at: 3 put: 3.0 ; yourself ]
]

{ #category : 'requirements' }
Float64ArrayTest >> subCollectionNotIn [
" return a collection for which at least one element is not included in 'moreThan4Elements' "
	^ collectionNotIncluded
		ifNil: [ collectionNotIncluded := (Float64Array new: 2) at:1 put: elementNotIn ; at: 2 put: elementNotIn  ; yourself ]
]

{ #category : 'requirements' }
Float64ArrayTest >> unsortedCollection [
" retur a collection that is not yat sorted"
	^nonEmpty5ElementsNoDuplicate
]

{ #category : 'requirements' }
Float64ArrayTest >> withEqualElements [
	^ collectionWithEqualElements
]
