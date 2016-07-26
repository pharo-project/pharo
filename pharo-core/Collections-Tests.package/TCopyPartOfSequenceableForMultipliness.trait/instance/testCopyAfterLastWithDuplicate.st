tests - copying part of sequenceable for multipliness
testCopyAfterLastWithDuplicate
	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining .
	element := collection  first.
	
	" collectionWithSameAtEndAndBegining first and last elements are equals.
	'copyAfter:' should copy after the last occurence of element :"
	result := collection   copyAfterLast: (element ).
	
	"verifying content: "
	self assert: result isEmpty.

