accessing
stream: aReadStream

	super stream: aReadStream.
	self stonReader on: stream.
