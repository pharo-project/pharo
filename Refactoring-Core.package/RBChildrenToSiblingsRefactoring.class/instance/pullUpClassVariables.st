private-variables
pullUpClassVariables
	| newSuperclass |
	newSuperclass := self abstractSuperclass.
	parent classVariableNames do: 
			[:each | 
			self performComponentRefactoring: (RBPullUpClassVariableRefactoring 
						model: self model
						variable: each
						class: newSuperclass)]