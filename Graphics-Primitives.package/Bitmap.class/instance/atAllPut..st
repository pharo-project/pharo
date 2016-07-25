accessing
atAllPut: value
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."
	<primitive: 145>
	super atAllPut: value.