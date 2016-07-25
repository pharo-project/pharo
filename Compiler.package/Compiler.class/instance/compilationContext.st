public - opal compatibility
compilationContext
	^ compilationContext ifNil: [  
		compilationContext := self compilationContextClass default
			requestor: requestor;
			class: class;
			category: category ]
			