tests
testNew
	self	should: [Context new: 5] raise: Error.
	[Context new: 5]
		ifError: [:error | error = 'Error: Contexts must only be created with newForMethod:'].
	[Context new]
		ifError: [:error  | error = 'Error: Contexts must only be created with newForMethod:'].	
	[Context basicNew]
		ifError: [:error | error = 'Error: Contexts must only be created with newForMethod:'].				

