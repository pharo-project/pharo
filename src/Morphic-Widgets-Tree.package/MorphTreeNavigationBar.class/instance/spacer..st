accessing
spacer: hsize
	^ (Morph new) color: Color transparent; extent: hsize @ (self hasSubmorphs  ifTrue: [self computedHeight] ifFalse: [(self preferedFont height + 8)]); yourself