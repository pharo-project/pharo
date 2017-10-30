evaluating
value: anObject1 value: anObject2
	"Masquerade as a two argument block, used by many of the sorting APIs, 
	by returning whether anObject1 should be placed before anObject2 or not"

	| result |
	result := self collate: anObject1 with: anObject2.
	^result <= 0