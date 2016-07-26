testing
hasTestSelectors

	^(self selectors anySatisfy: [ :each | 
		(each beginsWith: 'test') and: [
				each numArgs isZero 
			]
		]) or: [ 
			self superclass isTestCase and: [ 
				self shouldInheritSelectors and: [ 
						self superclass hasTestSelectors	
				]
			]
		]
	