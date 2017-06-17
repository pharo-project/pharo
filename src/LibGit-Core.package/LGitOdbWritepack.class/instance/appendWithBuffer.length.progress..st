accessing
appendWithBuffer: anAddress length: anInteger progress: aTransferProgress
	| data callout|
	data := self prim_append.
	callout := LGitWritepackAppendCallout fromHandle: data getHandle.

	^ callout
		writepack: self
		buffer: anAddress
		length: anInteger
		progress: aTransferProgress