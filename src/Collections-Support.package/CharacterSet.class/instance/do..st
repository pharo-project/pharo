collection ops
do: aBlock
	"evaluate aBlock with each character in the set"

	Character allByteCharacters do: [ :c |
		(self includes: c) ifTrue: [ aBlock value: c ] ]
