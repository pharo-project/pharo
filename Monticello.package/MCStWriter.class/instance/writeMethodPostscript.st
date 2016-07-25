writing
writeMethodPostscript
	self basicChunkContents: [:str | str space ].
	stream cr.