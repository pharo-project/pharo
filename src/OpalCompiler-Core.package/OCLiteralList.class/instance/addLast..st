adding
addLast: object
	"Only add if not already in list"

	(equalitySet includes: object) ifTrue: [^ object].
	equalitySet add: object.
	super addLast: object.
	^ object
