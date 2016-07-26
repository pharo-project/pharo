tests
testModelRecursiveMethodThatIsNotReferencedFromOtherMethods
	| class otherClass |
	class := model classNamed: #Object.
	class compile: 'someMethodThatIsNotReferenced ^2' classified: #(#accessing).
	self assert: (class definesMethod: #someMethodThatIsNotReferenced).
	otherClass := model metaclassNamed: self class superclass name.
	otherClass 
		compile: 'someMethodThatIsNotReferenced ^3 someMethodThatIsNotReferenced'
		classified: #(#accessing).
	model removeClassNamed: self class name.
	self executeRefactoring: (RBRemoveMethodRefactoring 
				model: model
				removeMethods: #(#someMethodThatIsNotReferenced)
				from: class).
	self deny: (class definesMethod: #someMethodThatIsNotReferenced)