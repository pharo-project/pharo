accessing
byteAt: index put: value
	<primitive: 61>
	self at: index put: value asCharacter.
	^value