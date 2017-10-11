accessing
updatePosition
	"Call this before doing character-based access"

	position := self atEnd ifFalse: [0]