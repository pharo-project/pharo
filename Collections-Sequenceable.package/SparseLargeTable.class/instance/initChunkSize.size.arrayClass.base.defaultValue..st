initialization
initChunkSize: aChunkSize size: aSize arrayClass: aClass base: b defaultValue: d

	| lastChunkSize |
	chunkSize := aChunkSize.
	size := aSize.
	base := b.
	defaultValue := d.
	1 to: (self basicSize - 1) do: [ :in | self basicAt: in put: (aClass new: chunkSize withAll: defaultValue) ].
	lastChunkSize := size \\ chunkSize.
	lastChunkSize = 0 ifTrue: [lastChunkSize := chunkSize].
	size = 0 
		ifTrue: [self basicAt: 1 put: (aClass new: 0)]
		ifFalse: [self basicAt: self basicSize put: (aClass new: lastChunkSize withAll: defaultValue)].
