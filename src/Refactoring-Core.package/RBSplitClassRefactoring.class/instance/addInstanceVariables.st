private-transforming
addInstanceVariables
	instanceVariables do: 
			[:each | 
			self performComponentRefactoring: (RBAddInstanceVariableRefactoring 
						model: self model
						variable: each
						class: newClass)]