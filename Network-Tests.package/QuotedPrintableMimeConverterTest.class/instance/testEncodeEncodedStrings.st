testing
testEncodeEncodedStrings

	"An = becomes =3D and is then not decodable any more (only if you know you have to do it twice ;-) )"
	self
		assert: (self encode: 'Z=FCrich')
		equals: 'Z=3DFCrich'