open/close
close

	fileHandle ifNil: [^ self].  "already closed"
	self primClose: fileHandle.
	Smalltalk unregisterExternalObject: semaphore.
	semaphore := nil.
	fileHandle := nil.
