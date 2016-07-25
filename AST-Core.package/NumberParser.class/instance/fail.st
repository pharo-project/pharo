error
fail
	failBlock ifNotNil: [^failBlock value].
	self error: 'Reading a number failed'