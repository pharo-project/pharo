accessing
srcOffset: anInteger
	"Set the srcOffset."

	srcOffset := anInteger.
	self mappings do: [:j |
		j srcOffset: anInteger]