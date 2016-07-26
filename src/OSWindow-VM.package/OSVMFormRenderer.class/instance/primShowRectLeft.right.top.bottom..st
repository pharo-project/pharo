private
primShowRectLeft: l right: r top: t bottom: b
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. If this fails, retry integer coordinates."

	<primitive: 127>
	"if this fails, coerce coordinates to integers and try again"
	self primRetryShowRectLeft: l truncated
		right: r rounded
		top: t truncated
		bottom: b rounded.
