simple ensure tests
testDoC
	"self run: #testDoC"
	
	[self doC: [ self error]] on: Error do: [:e|].
	"necessary for SUnit"
	self assert: x = 3
