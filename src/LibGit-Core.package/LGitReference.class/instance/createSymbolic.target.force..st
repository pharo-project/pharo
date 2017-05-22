operations
createSymbolic: nameString target: targetString force: aBoolean
	self withReturnHandlerDo: [ 
		self
			reference_symbolic_create: self
			repo: self repository
			name: nameString
			target: targetString
			force: aBoolean
			log_message: 'created reference ', nameString ]