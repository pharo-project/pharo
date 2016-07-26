simple ensure tests
testOnDo
	"self run: #testOnDo"
	
	self doOnDo: [ self error].
	self assert: x = 3.
	x := 77.
	self doOnDo: [x = 21].
	self assert: x = 3
