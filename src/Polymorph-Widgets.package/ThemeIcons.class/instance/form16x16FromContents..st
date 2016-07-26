utils
form16x16FromContents: aByteArray 
	^ Form
	extent: 16@16
	depth: 32
	fromArray: aByteArray 
	offset: 0@0