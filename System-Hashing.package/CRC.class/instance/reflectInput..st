accessing-parameters
reflectInput: aBoolean
	"Determines if each byte should be reflected before processing.
	If false, bit 7 will be treated as most significant bit.
	If true, each byte will be reflected (bit 0 will be most significant)."
	shouldReflectInput := aBoolean