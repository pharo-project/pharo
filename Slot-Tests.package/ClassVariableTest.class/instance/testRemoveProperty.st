tests  - properties
testRemoveProperty
	| classVariable |
	classVariable := self class classVariableNamed: #TestVariable.

	classVariable propertyAt: #testKeySelector put: 1.
	classVariable removeProperty: #testKeySelector.
	self
		assert: (classVariable propertyAt: #testKeySelector)
		equals: nil.
	self assert: classVariable properties isNil.