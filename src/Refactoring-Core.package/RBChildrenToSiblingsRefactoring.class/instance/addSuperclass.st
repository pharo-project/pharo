transforming
addSuperclass
	self performComponentRefactoring: (RBAddClassRefactoring 
				model: self model
				addClass: className
				superclass: parent superclass
				subclasses: (Array with: parent)
				category: parent category)