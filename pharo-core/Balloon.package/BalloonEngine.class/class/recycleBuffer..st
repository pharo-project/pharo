private
recycleBuffer: balloonBuffer
	"Try to keep the buffer for later drawing operations."

	CacheProtect critical:[ | buffer |
		buffer := BufferCache at: 1.
		(buffer isNil or:[buffer size < balloonBuffer size] )
			ifTrue:[BufferCache at: 1 put: balloonBuffer].
	].