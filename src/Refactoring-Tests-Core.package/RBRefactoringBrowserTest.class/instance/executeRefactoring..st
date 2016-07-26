private
executeRefactoring: aRefactoring 
	aRefactoring primitiveExecute.
	RBParser parseExpression: aRefactoring storeString