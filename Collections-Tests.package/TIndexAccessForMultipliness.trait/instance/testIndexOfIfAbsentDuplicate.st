tests - index accessing for multipliness
testIndexOfIfAbsentDuplicate
	"self debug: #testIndexOfIfAbsent"
	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'indexOf:ifAbsent: should return the position of the first occurrence :'"
	self assert: (collection 
			indexOf: element
			ifAbsent: [ 55 ]) = 1