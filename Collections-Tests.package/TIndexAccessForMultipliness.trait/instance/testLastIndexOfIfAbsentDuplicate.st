tests - index accessing for multipliness
testLastIndexOfIfAbsentDuplicate
	"self debug: #testIndexOfIfAbsent"
	"self debug: #testLastIndexOf"
	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf: should return the position of the last occurrence :'"
	self assert: (collection 
			lastIndexOf: element
			ifAbsent: [ 55 ]) = collection size