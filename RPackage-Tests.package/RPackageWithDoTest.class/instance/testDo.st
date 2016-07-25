simple ensure tests
testDo
	"self run: #testDo"
	
	[self do: [ Error signal]] on:Error do: [:ex|].
	"necessary else SUnit believe that the error is not handle"
	self assert: x = 3
