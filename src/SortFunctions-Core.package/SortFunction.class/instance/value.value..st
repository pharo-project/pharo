evaluating
value: anObject value: bObject
	"Masquerade as a two argument block, used by many of the sorting APIs, by returning whether anObject should be placed before bObject or not."

	| result |
	result := (collator value: anObject value: bObject) * direction.
	^result <= 0