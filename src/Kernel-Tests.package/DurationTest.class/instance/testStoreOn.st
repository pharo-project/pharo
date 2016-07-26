tests
testStoreOn
	| stream |
	aDuration storeOn: (stream := (String new: 20) writeStream).
	self assert: stream contents = '(Duration seconds: 93784 nanoSeconds: 5)'.

