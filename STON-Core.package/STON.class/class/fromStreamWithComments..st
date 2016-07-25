convencience
fromStreamWithComments: readStream
	^ (self reader on: (STONCStyleCommentsSkipStream on: readStream)) next