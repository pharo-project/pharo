*libgit-core
asLGitExternalString
	| data |
	data := ExternalData
		fromHandle: (ExternalAddress allocate: self size + 1)
		type: ExternalType char asPointerType.
	LibC memCopy: self to: data size: self size.
	data getHandle byteAt: self size + 1 put: 0.
	^ data