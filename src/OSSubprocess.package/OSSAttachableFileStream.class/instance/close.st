open/close
close
	"Close this file."

	| handle |
	(handle := self basicFileID) ifNotNil: [
		self primCloseNoError: handle.
		self unregister.
		fileID := nil
	]
