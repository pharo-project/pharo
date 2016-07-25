drawning
formFromContents: aByteArray
	| size |
	size := 16.
	^ Form
		extent: size@size
		depth: 32
		fromArray: aByteArray 
		offset: 0@0