column drawing
header
	^ header ifNil: [header :=  (Morph new extent: 0@0) color: Color transparent]