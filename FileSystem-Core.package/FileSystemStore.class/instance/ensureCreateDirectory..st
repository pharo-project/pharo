public
ensureCreateDirectory: aPath
	(self isDirectory: aPath) ifTrue: [ ^ self ].
	self ensureCreateDirectory: aPath parent.
	self createDirectory: aPath