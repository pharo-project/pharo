accessing
loadedCodeDescription
	self verifyDirectoryStructureIfMissing: [ :message | ^ message ].
	^ self repository loadedCode description