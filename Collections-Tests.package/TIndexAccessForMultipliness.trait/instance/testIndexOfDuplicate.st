tests - index accessing for multipliness
testIndexOfDuplicate
	"self debug: #testIndexOf"
	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'indexOf: should return the position of the first occurrence :'"
	self assert: (collection indexOf: element) = 1