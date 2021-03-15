"
I'm a refactoring operation for replace one method call by another one.

The another method name should have the same number of arguments.

All senders of this method are changed by the other

Example
-------
Script:
```
(RBReplaceMethodRefactoring  
	model: model
	replaceMethod: #anInstVar:
	in: RBBasicLintRuleTestData
	to: #newResultClass: 
	permutation: (1 to: 1)
	inAllClasses: true) execute
```
"
Class {
	#name : #RBReplaceMethodRefactoring,
	#superclass : #RBChangeMethodNameRefactoring,
	#instVars : [
		'replaceInAllClasses'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBReplaceMethodRefactoring class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap [ 
	^ self new
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
]

{ #category : #'instance creation' }
RBReplaceMethodRefactoring class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean [
	^ self new
		model: aRBSmalltalk;
		replaceCallMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap
			inAllClasses: aBoolean;
		yourself
]

{ #category : #'instance creation' }
RBReplaceMethodRefactoring class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap [ 
	^self new replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
]

{ #category : #'instance creation' }
RBReplaceMethodRefactoring class >> replaceCallMethod: aSelector in: aClass to: newSelector permutation: aMap inAllClasses: aBoolean [
	^self new replaceCallMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
		inAllClasses: aBoolean
]

{ #category : #transforming }
RBReplaceMethodRefactoring >> haveSameNumberOfArgs [
	(self numberOfArgs: oldSelector) = (self numberOfArgs: newSelector) 
		ifFalse: [ self refactoringFailure: 'The new selector does not have the same number of parameters.']
]

{ #category : #actions }
RBReplaceMethodRefactoring >> numberOfArgs: aSymbol [
	^ (aSymbol asString splitOn: ':' ) size
]

{ #category : #transforming }
RBReplaceMethodRefactoring >> preconditions [ 
	|conditions|
	conditions := (RBCondition withBlock: [ self haveSameNumberOfArgs. true])
						&(RBCondition definesSelector: oldSelector in: class )
						& (RBCondition definesSelector: newSelector in: class ).
	
	^ conditions & (RBCondition withBlock: 
		[ |senders|
		senders := self replaceInAllClasses 
			ifTrue: [ self model allReferencesTo: oldSelector ]
			ifFalse: [ self model allReferencesTo: oldSelector in: {class} ].
		senders size > 1 
			ifTrue: 
				[self refactoringWarning: ('This will modify all <1p> senders.<n>Proceed anyway?' 
							expandMacrosWith: senders size)].
		true])
]

{ #category : #initialization }
RBReplaceMethodRefactoring >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap [
	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap.
]

{ #category : #initialization }
RBReplaceMethodRefactoring >> replaceCallMethod: aSelector in: aClass to: newSel permutation: aMap inAllClasses: aBoolean [
	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
	permutation := aMap.
	replaceInAllClasses := aBoolean.
]

{ #category : #accessing }
RBReplaceMethodRefactoring >> replaceInAllClasses [
	^ replaceInAllClasses ifNil: [ replaceInAllClasses := false ]
]

{ #category : #transforming }
RBReplaceMethodRefactoring >> transform [
	self replaceInAllClasses 
		ifTrue: [ self renameMessageSends ]
		ifFalse: [ self renameMessageSendsIn: {class} ]
]
