tests - copying part of sequenceable for multipliness
testCopyUpToLastWithDuplicate
	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining .
	element := collection  first.
	
	" collectionWithSameAtEndAndBegining first and last elements are equals.
	'copyUpToLast:' should copy until the last occurence :"
	result := collection   copyUpToLast: (element ).
	
	"verifying content: "
	1 to: result size do:
		[:i |
		self assert: (result at: i ) = ( collection at: i )  
		].
	
	self assert: result size = (collection size - 1).

