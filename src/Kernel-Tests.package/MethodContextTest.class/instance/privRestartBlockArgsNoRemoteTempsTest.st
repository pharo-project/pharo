private
privRestartBlockArgsNoRemoteTempsTest
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"
	
	self assert: 30 equals: ([:a :first | |b| 
		self assert: 10 = a .
		self assert: nil == b.
		b := a + 20. 
		first ifTrue: [
			"Cheat and modify one of the args so we will not loop endlessly"
			 thisContext tempAt: 2 put: false.
			thisContext restart.].
		b] value: 10 value: true)
