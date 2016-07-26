printing
storeStringBase: base
	^ String streamContents: [:strm | self storeOn: strm base: base]