tests
testBinding
	self assert: Object binding value = Object.
	self assert: Object binding key = #Object.
	
	self assert: Object class binding value = Object class.
	
	"returns nil for Metaclasses... like Encoder>>#associationFor:"
	
	self assert: Object class binding key isNil.