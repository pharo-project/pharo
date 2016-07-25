private-variables
pullUpInstanceVariables
	| newSuperclass |
	newSuperclass := self abstractSuperclass.
	parent instanceVariableNames do: 
			[:each | 
			self performComponentRefactoring: (RBPullUpInstanceVariableRefactoring 
						model: self model
						variable: each
						class: newSuperclass)]