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
FloatArrayTest >> testArithmeticCoercion [
	"This test is related to http://bugs.squeak.org/view.php?id=6782"
	
	self should: [3.0 / (FloatArray with: 2.0) = (FloatArray with: 1.5)].
	self should: [3.0 * (FloatArray with: 2.0) = (FloatArray with: 6.0)].
	self should: [3.0 + (FloatArray with: 2.0) = (FloatArray with: 5.0)].
	self should: [3.0 - (FloatArray with: 2.0) = (FloatArray with: 1.0)].
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

{ #category : #requirements }
FloatArrayTest >> unsortedCollection [
" retur a collection that is not yat sorted"
	^nonEmpty5ElementsNoDuplicate 
]

{ #category : #requirements }
FloatArrayTest >> withEqualElements [
	^ collectionWithEqualElements 
]
