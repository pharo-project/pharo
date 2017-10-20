open/close
ensureOpen
	"Make sure that this file really is open."

	self closed ifTrue: [^ self reopen].
	(self primSizeNoError: fileID) ifNotNil: [^ self].
	self reopen.
