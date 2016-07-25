private
streamContents: block
	"Like readStream collectionSpecies streamContents: block
	but reusing the underlying buffer for improved efficiency"
	
	bufferStream 
		ifNil: [ 
			bufferStream := (readStream collectionSpecies new: 32) writeStream ].
	bufferStream reset.
	block value: bufferStream.
	^ bufferStream contents