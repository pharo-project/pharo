private
ruleFor: aSelector plusSelectors: symbolCollection 
	| returnedEnvironment rule class selector className |
	selector := aSelector asString 
		copyFrom: 5
		to: aSelector size.
	className := 'RB' , selector , 'Rule'.
	class := Smalltalk classNamed: className.
	class isNil ifTrue: [ self error: className , ' class not found' ].
	selector 
		at: 1
		put: selector first asLowercase.
	selector := selector asSymbol.
	RBSmalllintChecker 
		runRule: (rule := class new)
		onEnvironment: self smalllintTestEnvironment.
	returnedEnvironment := RBSelectorEnvironment new.
	returnedEnvironment 
		addClass: RBSmalllintTestObject
		selector: selector.
	symbolCollection do: [ :each | 
		returnedEnvironment 
			addClass: RBSmalllintTestObject
			selector: each ].
	self
		assert: (RBSmalllintTestObject includesSelector: selector)
		description: 'Missing test method RBSmalllintTestObject>>', selector printString.
		
	self 
		assert: (rule name isString and: [ rule name notEmpty ])
		description: 'Missing rule name'.
	self 
		assert: (rule group isString and: [ rule group notEmpty ])
		description: 'Missing group name'.
	self 
		assert: (rule rationale isString and: [ rule rationale notEmpty ])
		description: 'Missing rationale'.
	self
		assert: (#(error warning information) includes: rule severity)
		description: 'Invalid severity'.
	self
		checkRule: rule
		isEqualTo: returnedEnvironment