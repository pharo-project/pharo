private
allocateOrRecycleBuffer: initialSize
	"Try to recycly a buffer. If this is not possibly, create a new one."
	| buffer |
	CacheProtect critical:[
		buffer := BufferCache at: 1.
		BufferCache at: 1 put: nil.
	].
	^buffer ifNil:[BalloonBuffer new: initialSize]