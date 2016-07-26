accessing
at: anInteger
	"Answer my element at index anInteger. at: is used by a knowledgeable
	client to access an existing element"

	| index |
	anInteger < 1
		ifTrue: [ self errorSubscriptBounds: anInteger ].
	(index := anInteger + firstIndex - 1) > lastIndex
		ifTrue: [ self errorSubscriptBounds: anInteger ].
	^ array at: index