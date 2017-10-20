running
setUp

	
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