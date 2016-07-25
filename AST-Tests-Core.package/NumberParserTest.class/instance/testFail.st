tests - fail
testFail
	"Verify that the value of a failblock is returned."
	self assert: (NumberParser parse: 'blablabla' onError: [42]) equals: 42