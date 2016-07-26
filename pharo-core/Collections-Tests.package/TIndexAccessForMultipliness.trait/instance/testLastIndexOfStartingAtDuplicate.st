tests - index accessing for multipliness
testLastIndexOfStartingAtDuplicate
	"self debug: #testLastIndexOf"
	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf:ifAbsent:startingAt: should return the position of the last occurrence :'"
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 55 ]) = collection size.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 55 ]) = 1