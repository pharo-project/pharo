initialize-destroy
destroy
	"Destroy this socket. Its connection, if any, is aborted and its resources are freed. Do nothing if the socket has already been destroyed (i.e., if its socketHandle is nil)."

	socketHandle 
		ifNotNil: [ 
			self isValid
				ifTrue: [ self primSocketDestroy: socketHandle ].
			Smalltalk unregisterExternalObject: semaphore.
			Smalltalk unregisterExternalObject: readSemaphore.
			Smalltalk unregisterExternalObject: writeSemaphore.
			socketHandle := nil.
			readSemaphore := writeSemaphore := semaphore := nil.
			self unregister ]