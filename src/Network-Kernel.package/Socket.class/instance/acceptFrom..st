initialize-destroy
acceptFrom: aSocket
	"Initialize a new socket handle from an accept call"

	| semaIndex readSemaIndex writeSemaIndex |
	socketHandle ifNotNil: [^Error signal: 'The socket is already bound'].
	semaphore := Semaphore new.
	readSemaphore := Semaphore new.
	writeSemaphore := Semaphore new.
	semaIndex := Smalltalk registerExternalObject: semaphore.
	readSemaIndex := Smalltalk registerExternalObject: readSemaphore.
	writeSemaIndex := Smalltalk registerExternalObject: writeSemaphore.
	socketHandle := self
		primAcceptFrom: aSocket socketHandle
		receiveBufferSize: 8000
		sendBufSize: 8000
		semaIndex: semaIndex
		readSemaIndex: readSemaIndex
		writeSemaIndex: writeSemaIndex.
	socketHandle 
		ifNil: [ 
			"socket creation failed"
			Smalltalk unregisterExternalObject: semaphore.
			Smalltalk unregisterExternalObject: readSemaphore.
			Smalltalk unregisterExternalObject: writeSemaphore.
			readSemaphore := writeSemaphore := semaphore := nil ]
		ifNotNil: [ self register ]