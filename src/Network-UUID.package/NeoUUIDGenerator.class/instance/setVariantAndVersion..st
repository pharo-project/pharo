private
setVariantAndVersion: uuid
	"Set the version (top four most significant bits) to 0, an undefined version in RFC 4122"
	uuid at: 7 put: ((uuid at: 7) bitAnd: 2r1111).
	"Set the variant (top two most significant bits) to '10', the RFC 4122 variant"
	uuid at: 9 put: (((uuid at: 9) bitAnd: 2r111111) bitOr: 2r10000000)