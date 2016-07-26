instance creation
new: size chunkSize: chunkSize arrayClass: aClass base: b defaultValue: d

	| basicSize |
	(basicSize := ((size - 1) // chunkSize) + 1) = 0
		ifTrue: [basicSize := 1].
	^(self basicNew: basicSize)
		initChunkSize: chunkSize size: size arrayClass: aClass base: b defaultValue: d;
		yourself
