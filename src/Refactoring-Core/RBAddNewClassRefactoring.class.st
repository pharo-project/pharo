"
This refactorings differs from the Insert New Class refactoring in that does not reparent the subclasses to the new class.
"
Class {
	#name : 'RBAddNewClassRefactoring',
	#superclass : 'RBInsertNewClassRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBAddNewClassRefactoring >> applicabilityPreconditions [

	^ { 
		(RBCondition isValidClassName: className) .
		(RBCondition isGlobal: className in: self model) not .
		(RBCondition isSymbol: packageName) .
		((RBCondition withBlock: [ packageName isNotEmpty ]) errorMacro: 'Invalid package name')
		}
]

{ #category : 'transforming' }
RBAddNewClassRefactoring >> privateTransform [

	self model
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: superclass name;
				name: className;
				package: packageName;
				tag: tagName;
				comment: self comment ]
]
