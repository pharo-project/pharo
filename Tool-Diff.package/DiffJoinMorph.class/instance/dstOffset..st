accessing
dstOffset: anInteger
	"Set the dstOffset."

	dstOffset := anInteger.
	self mappings do: [:j |
		j dstOffset: anInteger]