tests - index accessing for multipliness
testIndexOfStartingAtDuplicate
	"self debug: #testLastIndexOf"
	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'indexOf:ifAbsent:startingAt: should return the position of the first occurrence :'"
	self assert: (collection 
			indexOf: element
			startingAt: 1
			ifAbsent: [ 55 ]) = 1.
	self assert: (collection 
			indexOf: element
			startingAt: 2
			ifAbsent: [ 55 ]) = collection size