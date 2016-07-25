tests - copying part of sequenceable for multipliness
testCopyUpToWithDuplicate
	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining .
	element := collection  last.
	
	" collectionWithSameAtEndAndBegining first and last elements are equals.
	'copyUpTo:' should copy until the first occurence :"
	result := collection   copyUpTo: (element ).
	
	"verifying content: "
	self assert: result isEmpty.

