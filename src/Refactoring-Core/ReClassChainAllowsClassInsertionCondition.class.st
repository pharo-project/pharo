"
Given a superclass and subclasses, this class implements a precondition check for a refactoring operation, ensuring that the superclass is not a metaclass and that all subclasses are immediate subclasses of the superclass, thus maintaining the integrity of the class hierarchy during the refactoring process.

This ensures that all conditions must be true for the refactoring to proceed. If any condition fails, the entire refactoring operation is considered not applicable, and the refactoring tool would typically halt the operation or notify the user of the issue.
"
Class {
	#name : 'ReClassChainAllowsClassInsertionCondition',
	#superclass : 'ReClassesCondition',
	#instVars : [
		'superclass',
		'subclasses'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReClassChainAllowsClassInsertionCondition >> superclass: aRBClass [ 
	superclass := aRBClass
]

{ #category : 'accessing' }
ReClassChainAllowsClassInsertionCondition >> violators [ 

	^ violators ifNil: [
		  violators := superclass isMeta
			ifTrue: [ { superclass } ] 
			ifFalse: [ #() ] ]

]
