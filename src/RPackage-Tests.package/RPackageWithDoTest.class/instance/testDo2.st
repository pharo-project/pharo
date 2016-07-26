simple ensure tests
testDo2
	"self run: #testDo2"
	
	[self do: [ self error]
		] on: Error do: [:ex | ].
	self assert: x = 3
