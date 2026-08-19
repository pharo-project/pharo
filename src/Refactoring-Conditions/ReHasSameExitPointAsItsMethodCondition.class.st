"
I check if a subtree changes the execution flow of its method. 
This tipically occurs when the subtree contains blocks with return expressions
"
Class {
	#name : 'ReHasSameExitPointAsItsMethodCondition',
	#superclass : 'ReSubtreeCondition',
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReHasSameExitPointAsItsMethodCondition >> check [

	^ subtree hasSameExitPoint
]

{ #category : 'displaying' }
ReHasSameExitPointAsItsMethodCondition >> violationMessageOn: aStream [ 

	^ aStream nextPutAll: 'Extracting guard clauses and other expressions that directly affect a method’s execution flow may cause unexpected behavior in the original method.'
]
