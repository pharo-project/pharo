private
stringStreamContents: block
	stringStream ifNil: [ 
		stringStream := (String new: 32) writeStream ].
	stringStream reset.
	block value: stringStream.
	^ stringStream contents