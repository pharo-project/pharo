running
tearDown
	clientStream ifNotNil:[clientStream destroy].
	serverStream ifNotNil:[serverStream destroy].