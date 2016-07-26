options
streaming
	"Return whether I will return streaming response objects (see ZnStreamingEntity).
	If true, my users are responsible for actually reading the incoming response entity data stream."
	
	^ self optionAt: #streaming ifAbsent: [ false ]