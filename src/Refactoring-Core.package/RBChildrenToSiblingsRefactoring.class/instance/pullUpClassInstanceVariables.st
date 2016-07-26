private-variables
pullUpClassInstanceVariables
	| newSuperclass |
	newSuperclass := self abstractSuperclass theMetaClass.
	parent theMetaClass instanceVariableNames do: 
		[ :each | 
		self performComponentRefactoring: (RBPullUpInstanceVariableRefactoring 
				model: self model
				variable: each
				class: newSuperclass) ]