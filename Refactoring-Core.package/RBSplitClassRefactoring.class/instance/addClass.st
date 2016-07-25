private-transforming
addClass
	self performComponentRefactoring: (RBAddClassRefactoring 
				model: self model
				addClass: newClassName
				superclass: Object
				subclasses: #()
				category: class category).
	newClass := self model classNamed: newClassName asSymbol