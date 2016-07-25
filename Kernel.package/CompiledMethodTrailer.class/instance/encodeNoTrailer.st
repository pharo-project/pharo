encoding
encodeNoTrailer

	"When creating methods with no trailer, male them, 4 bytes long so that we can update them to sourcePointers 
	without having to use #become:"

	size := 4.
	encodedData := #[0 0 0 0].