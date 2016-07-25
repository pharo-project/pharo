tests - properties
testPropertyAtPut

	| testValue package |
	
	testValue := Date today.
	package := self class package.
	
	package propertyAt: #testKeySelector put: testValue.
	self
		assert: (package propertyAt: #testKeySelector)
		equals: testValue.

	package removeProperty: #testKeySelector.
	self assert: package properties isNil.