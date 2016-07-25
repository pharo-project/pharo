tests
testEqual
	"self debug: #testEqual"
	
	self assert: aTime = (Time readFrom: '12:34:56' readStream).
	self assert: aTime = (Time readFrom: '12:34:56.00' readStream).
	self assert: aTime = (Time readFrom: '12:34:56.0000' readStream)