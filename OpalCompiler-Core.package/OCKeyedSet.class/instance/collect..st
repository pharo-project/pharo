copying
collect: aBlock 
	"OCKeyedSet>>collect: must return an instance of Set and not an instance of his own class"

	| newSet |
	newSet := Set new: self size.
	array do: [:each | each ifNotNil: [newSet add: (aBlock value: each enclosedSetElement)]].
	^ newSet