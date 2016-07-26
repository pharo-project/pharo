tests
testSelector
	"self debug: #testSelector"
	"The selector of a compiled method should be its name. 
	An aliased method should have the name of its alias name."
	
	self assert: (MOPTestClassA>>#c) selector = #c.
	self assert: (MOPTestClassC>>#c) selector = #c.
	self assert: (Trait3>>#c) selector = #c.
	self assert: (Trait3>>#c2) selector = #c2.
	self assert: (MOPTestClassD>>#c3) selector = #c3.