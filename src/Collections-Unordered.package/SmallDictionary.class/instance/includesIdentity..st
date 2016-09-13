testing
includesIdentity: aValue
	"Answer whether aValue is one of the values of the receiver.  Contrast #includes: in which there is only an equality check, here there is an identity check"

	self do: [:each | aValue == each ifTrue: [^ true]].
	^ false