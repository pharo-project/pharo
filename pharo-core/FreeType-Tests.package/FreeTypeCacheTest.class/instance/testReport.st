tests
testReport
	self assert: fullCache report = '100% Full (maximumSize: 400320 , used: 400320)'.
	fullCache maximumSize:  800640.
	self assert: fullCache report = '50% Full (maximumSize: 800640 , used: 400320)'.
	self assert: cache report = '0% Full (maximumSize: 5120000 , used: 0)'.
	cache maximumSize: nil.
	self assert: cache report = '0% Full (maximumSize: nil , used: 0)'.	