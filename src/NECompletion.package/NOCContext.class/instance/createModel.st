model creation
createModel
	| aModel |
	
	aModel := NOCModel
		class: theClass
		temporaries: self temporaries
		additionals: controller additionals
		variables: variables
		selectors: selectors.
	aModel
		context: self.
		
	^ aModel 