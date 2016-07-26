accessing
nextOrNilSuchThat: aBlock
	"Answer the next object that satisfies aBlock, skipping any intermediate objects.
	If no object has been sent, answer <nil> and leave me intact.
	NOTA BENE:  aBlock MUST NOT contain a non-local return (^)."

	^monitor critical: [
		| index |
		index := items findFirst: aBlock.
		index = 0
			ifTrue: [nil]
			ifFalse: [items removeAt: index]
	]