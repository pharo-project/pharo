accessing
atWrap: index 
	"Answer the index'th element of the receiver.  If index is out of bounds,
	let it wrap around from the end to the beginning until it is in bounds."

	^ self at: index - 1 \\ self size + 1