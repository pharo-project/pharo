tests
testData
	"It is really important that it is the same object."
	bundle data: data.
	self assert: bundle data == bundle data.