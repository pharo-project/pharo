open/close
reopen
	"Close and reopen this file. The file position is reset to zero."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	fileID ifNotNil: [
		collection ifNotNil: [
			position < readLimit ifTrue: [
				self flushReadBuffer ] ].
		self primCloseNoError: fileID ].
	self open: name forWrite: rwmode.
