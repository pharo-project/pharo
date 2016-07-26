accessing
bytes
	| uncompressed |
	uncompressed := self readFromMember.
	self updateSize: uncompressed size.
	^ uncompressed