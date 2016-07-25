options
streaming: boolean
	"Set whether I will return streaming response objects (see ZnStreamingEntity) to boolean.
	If true, my users are responsible for actually reading the incoming response entity data stream.
	I cannot do streaming when #isOneShot is true because the stream would be closed too soon."

	(boolean and: [ self isOneShot ])
		ifTrue: [ self error: 'I cannot do streaming when #isOneShot is true.' ].
	self optionAt: #streaming put: boolean