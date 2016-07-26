enumerating
noneSatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for all elements return true.
	Otherwise return false"

	self do: [:item | (aBlock value: item) ifTrue: [^ false]].
	^ true