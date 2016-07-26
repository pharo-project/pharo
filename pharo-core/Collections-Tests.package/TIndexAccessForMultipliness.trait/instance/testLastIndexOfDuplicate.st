tests - index accessing for multipliness
testLastIndexOfDuplicate
	"self debug: #testLastIndexOf"
	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf: should return the position of the last occurrence :'"
	self assert: (collection lastIndexOf: element) = collection size