private
assertIsValidRule: aRule
	self
		assert: (aRule name isString and: [ aRule name notEmpty ])
		description: 'Missing rule name'.
	self
		assert: (aRule group isString and: [ aRule group notEmpty ])
		description: 'Missing group name'.
	self
		assert: (aRule rationale isString and: [ aRule rationale notEmpty ])
		description: 'Missing rationale'.
	self
		assert: (#(error warning information) includes: aRule severity)
		description: 'Invalid severity'