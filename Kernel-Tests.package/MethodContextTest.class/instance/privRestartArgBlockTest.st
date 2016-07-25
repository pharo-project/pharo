private
privRestartArgBlockTest
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"
	|firstTimeThrough |
	firstTimeThrough := true.
	
	self assert: 30 equals: ([:a | |b| 
		self assert: 10 = a .
		self assert: nil == b.
		b := a + 20. 
		firstTimeThrough ifTrue: [
			firstTimeThrough := false.
			thisContext restart.].
		b] value: 10)
