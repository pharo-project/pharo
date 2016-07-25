tests
testMethodClassBindingShouldBeClassBinding
	"A method should have the same binding than its class"
	self assert: (self methodBinding == class binding).
	
	