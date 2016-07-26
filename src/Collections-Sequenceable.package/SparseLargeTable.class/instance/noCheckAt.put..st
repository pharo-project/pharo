accessing
noCheckAt: index put: value

	| chunkIndex t |
	chunkIndex := index - base // chunkSize + 1.
	chunkIndex > self basicSize ifTrue: [^ value].
	t :=  self basicAt: chunkIndex.
	t ifNil: [^ value].
	^ t at: (index - base + 1 - (chunkIndex - 1 * chunkSize)) put: value
