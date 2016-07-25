stream creation
new: size streamContents: aBlock
	^ self withAll: (super new: size streamContents: aBlock)