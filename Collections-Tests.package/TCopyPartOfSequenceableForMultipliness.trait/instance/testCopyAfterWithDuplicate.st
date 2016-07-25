tests - copying part of sequenceable for multipliness
testCopyAfterWithDuplicate
	| result element  collection |
	collection := self collectionWithSameAtEndAndBegining .
	element := collection  last.
	
	" collectionWithSameAtEndAndBegining first and last elements are equals.
	'copyAfter:' should copy after the first occurence :"
	result := collection   copyAfter: (element ).
	
	"verifying content: "
	1 to: result size do: 
		[:i |
		self assert: (collection  at:(i + 1 )) = (result at: (i))
		].

	"verify size: "
	self assert: result size = (collection size - 1).