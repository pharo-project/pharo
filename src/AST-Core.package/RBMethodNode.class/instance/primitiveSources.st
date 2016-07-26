accessing
primitiveSources
	^ self pragmas collect: [ :each | self source copyFrom: each first to: each last ]