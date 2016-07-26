tests
testTraitMethodClassBindingShouldBeClassBinding
	"A method should have the same binding than its class even if it come from a trait"
	self assert: (self traitMethodBinding == class binding).
	
	