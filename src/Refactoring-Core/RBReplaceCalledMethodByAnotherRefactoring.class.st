"
I'm a refactoring operation for replace one method call by another one.

The another method name should have the same number of arguments.

All senders of this method are changed by the other
"
Class {
	#name : #RBReplaceCalledMethodByAnotherRefactoring,
	#superclass : #RBChangeMethodNameRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBReplaceCalledMethodByAnotherRefactoring class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap [ 
	^(self new)
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
]

{ #category : #'as yet unclassified' }
RBReplaceCalledMethodByAnotherRefactoring class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap [ 
	^self new replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
]

{ #category : #transforming }
RBReplaceCalledMethodByAnotherRefactoring >> haveSameNumberOfArgs [
	(self numberOfArgs: oldSelector) = (self numberOfArgs: newSelector) 
		ifFalse: [ self refactoringFailure: 'The new selector does not have the same number of parameters.']
]

{ #category : #'as yet unclassified' }
RBReplaceCalledMethodByAnotherRefactoring >> numberOfArgs: aSymbol [
	^ (aSymbol asString splitOn: ':' ) size
]

{ #category : #transforming }
RBReplaceCalledMethodByAnotherRefactoring >> preconditions [ 
	|conditions|
	conditions := (RBCondition withBlock: [ self haveSameNumberOfArgs. true])
						&(RBCondition definesSelector: oldSelector in: class )
						& (RBCondition definesSelector: newSelector in: class ).
	
	^ conditions & (RBCondition withBlock: 
		[ |senders|
		senders := (self model allReferencesTo: oldSelector).
		senders size > 1 
			ifTrue: 
				[self refactoringWarning: ('This will modify all <1p> senders.' 
							expandMacrosWith: senders size)].
		true])
]

{ #category : #'as yet unclassified' }
RBReplaceCalledMethodByAnotherRefactoring >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap [
	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap
]

{ #category : #transforming }
RBReplaceCalledMethodByAnotherRefactoring >> transform [
	self renameMessageSends
]
