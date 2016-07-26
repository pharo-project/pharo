testing
generateTestData: size
	"self generateTestData: 1111"
	
	^ String 
		streamContents: [ :stream |
			stream << 'Transfer test file of size '; print: size; << ' bytes.'; lf.
			1 to: size do: [ :each |
				stream nextPutAll: 'Line '; print: each; lf ] ] 
		limitedTo: size 