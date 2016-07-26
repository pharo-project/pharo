tests
testAddSeconds
	self assert: (aTime addSeconds: 1) = (Time readFrom: '12:34:57' readStream).
	self assert: (aTime addSeconds: 60) = (Time readFrom: '12:35:56' readStream).
	self assert: (aTime addSeconds: 3600) = (Time readFrom: '13:34:56' readStream).
	self assert: (aTime addSeconds: 24 * 60 * 60) = (Time readFrom: '12:34:56' readStream)