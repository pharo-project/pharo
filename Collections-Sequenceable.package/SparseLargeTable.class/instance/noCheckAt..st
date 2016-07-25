accessing
noCheckAt: index
	| chunkIndex t |

	chunkIndex := index - base // chunkSize + 1.
	(chunkIndex > self basicSize or: [chunkIndex < 1]) ifTrue: [^ defaultValue].
	t := self basicAt: chunkIndex.
	t ifNil: [^ defaultValue].
	^ t at: (index - base + 1 - (chunkIndex - 1 * chunkSize))
