transforming
removeMethod
	self onError: 
			[self performComponentRefactoring: (RBRemoveMethodRefactoring 
						model: self model
						removeMethods: (Array with: selector)
						from: class)]
		do: []