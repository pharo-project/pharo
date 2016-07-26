open/close
open: fullFileName forWrite: aBoolean
	"Open a file of the given name, and return a handle for that file. Answer the receiver if the primitive succeeds, nil otherwise.
	If openForWrite is true, then:
		if there is no existing file with this name, then create one
		else open the existing file in read-write mode
	otherwise:
		if there is an existing file with this name, then open it read-only
		else answer nil."
	"Note: if an exisiting file is opened for writing, it is NOT truncated. If truncation is desired, the file should be deleted before being opened as an asynchronous file."
	"Note: On some platforms (e.g., Mac), a file can only have one writer at a time."

	| semaIndex |
	name := fullFileName.
	writeable := aBoolean.
	semaphore := Semaphore new.
	semaIndex := Smalltalk registerExternalObject: semaphore.
	fileHandle := self primOpen: name asVmPathName forWrite: writeable semaIndex: semaIndex.
	fileHandle ifNil: [
		Smalltalk unregisterExternalObject: semaphore.
		semaphore := nil.
		^ nil].
