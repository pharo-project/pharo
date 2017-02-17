private
privRestartBlockTest
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"
	|a firstTimeThrough |
	firstTimeThrough := true.
	a := 10.
	
	self assert: 30 equals: [|b| 
		self assert: 10 = a .
		self assert: nil == b.
		b := a + 20. 
		firstTimeThrough ifTrue: [
			firstTimeThrough := false.
			thisContext restart.].
		b] value
