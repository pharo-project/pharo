"
I am a refactoring operation for renaming methods.

The new method name has to have the same number of arguments, but the order of arguments can be changed.

My preconditions verify that the number of arguments is the same and that the new method name isn't already used.

All references in senders of the old method are changed, either the method name only or the order of the supplied arguments.

Example
--------
There are two ways to rename a method, one of them is rename all senders of method:
```
(RBRenameMethodRefactoring 
		renameMethod: ('check', 'Class:') asSymbol
		in: RBBasicLintRuleTestData
		to: #checkClass1:
		permutation: (1 to: 1)) execute.
```
And the other is rename the method only in specific packages:
```
|refactoring|
refactoring :=RBRenameMethodRefactoring 
		renameMethod: ('check', 'Class:') asSymbol
		in: RBBasicLintRuleTestData
		to: #checkClass1:
		permutation: (1 to: 1).
refactoring searchInPackages:  #(#'Refactoring-Tests-Core').
refactoring execute
```
"
Class {
	#name : 'ReRenameMethodRefactoring',
	#superclass : 'RBChangeMethodNameRefactoring',
	#instVars : [
		'hasPermutedArguments',
		'violations'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
ReRenameMethodRefactoring class >> model: aRBSmalltalk renameMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^ self new
		model: aRBSmalltalk;
		renameMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
]

{ #category : 'instance creation' }
ReRenameMethodRefactoring class >> renameMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^ self new
		renameMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> applicabilityPreconditions [

	^ { self newNameDoesNotRequireRefactoringPrecondition not . 
		 (ReBlockCondition new
		   block: [ self haveSelectorsSameArity ];
		   violatorErrorString: newSelector printString
			   , ' doesn''t have the same number of arguments than ', oldSelector)}
]

{ #category : 'torevisit' }
ReRenameMethodRefactoring >> areArgumentsPermuted [
	"I fedup and not happy but let us move on. We may turn them into first class condition later."
	
	^ (permutation asArray = (1 to: oldSelector numArgs) asArray ) not
]

{ #category : 'testing' }
ReRenameMethodRefactoring >> areNamesTheSame [

	^ newSelector = oldSelector
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> breakingChangePreconditions [

	^ self doesNotOverrideExistingMethodPreconditions isEmpty
]

{ #category : 'printing' }
ReRenameMethodRefactoring >> breakingChangeViolationMessageOn: aStream [
	
	self violations size = 1
		ifTrue: [ aStream nextPutAll: violations first name; nextPutAll: ' defines the method ' ]
		ifFalse: [ 
	self violations 
			do: [ :each | aStream nextPutAll: each name ] 
			separatedBy: [ aStream nextPutAll: ', '].
	aStream nextPutAll: ' define the method '. ].
	aStream nextPutAll: newSelector.
]

{ #category : 'interactive' }
ReRenameMethodRefactoring >> doesNotOverrideExistingMethodPreconditions [
	"Check that the new selector is not already defined in class or superclasses of the implementors of the oldSelector."
	
	self implementors asOrderedCollection do: [ :each | 
			| cond |
			cond := (ReUpToRootDefinesMethod new class: each ; selector: newSelector).
			cond check
				ifTrue: [ self violations addAll: cond violators ] ].
	^ self violations
	
]

{ #category : 'redefined locally waiting for superclass migration' }
ReRenameMethodRefactoring >> generateChanges [
	"Prepare, check the preconditions, and perform the actual transformation (i.e., creating a list of changes that can be accessible using the changes message). This method should not be invoked from drivers since drivers usually check in finer grained mode the preconditions - and that this stage they already checked the preconditions."
	
	self prepareForExecution.

	(self applicabilityPreconditions reject: [ :each | each check ]) ifNotEmpty: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityPreconditions errorString ].
	self breakingChangePreconditions ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			(String streamContents: [ :s | self breakingChangeViolationMessageOn: s  ])].
	self privateTransform.
	^ self changes
]

{ #category : 'torevisit' }
ReRenameMethodRefactoring >> hasPermutedArguments [
	"We will have to unify with areArgumentsPermuted"
	
	^ hasPermutedArguments
		ifNil: [ hasPermutedArguments := super hasPermutedArguments ]
		ifNotNil: [ hasPermutedArguments ]
]

{ #category : 'testing' }
ReRenameMethodRefactoring >> haveSelectorsSameArity [
	
	^ oldSelector numArgs = newSelector numArgs
]

{ #category : 'testing' }
ReRenameMethodRefactoring >> implementorsCanBePrimitives [
	^self hasPermutedArguments not
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> localConditions [
	"For a rename we only check for the same arity, in add parameter obviously it should be different"
	
	^ ReBlockCondition new 
		block: [ self haveSelectorsSameArity ];
		violatorErrorString: newSelector printString
				, ' doesn''t have the same number of arguments as ', oldSelector printString
]

{ #category : 'private' }
ReRenameMethodRefactoring >> modifyImplementorParseTree: parseTree in: aClass [
	super modifyImplementorParseTree: parseTree in: aClass.
	self renameArgumentsIn: parseTree
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> newNameDoesNotRequireRefactoringPrecondition [

	^ ReBlockCondition new
		   block: [ self areNamesTheSame & self areArgumentsPermuted not ];
		   violatorErrorString:
		   'The selector ', newSelector, ' is unchanged and its arguments are not permuted'
		   
]

{ #category : 'private' }
ReRenameMethodRefactoring >> parseTreeRewriterInstance [

	^ self hasPermutedArguments
		  ifTrue: [ self parseTreeRewriterClass new ]
		  ifFalse: [
			  self parseTreeRewriterClass
				  replaceLiteral: oldSelector
				  with: newSelector ]
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> preconditions [
	self halt.
	
	^ self newNameDoesNotRequireRefactoringPrecondition | self superPreconditions
]

{ #category : 'accessing' }
ReRenameMethodRefactoring >> refactoredClass [
	^ class
]

{ #category : 'changes' }
ReRenameMethodRefactoring >> renameChanges [

	self privateTransform.
	^ self changes
]

{ #category : 'printing' }
ReRenameMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream
		nextPutAll: ' renameMethod: #';
		nextPutAll: oldSelector;
		nextPutAll: ' in: '.
	aStream nextPutAll: class name.	
	aStream
		nextPutAll: ' to: #';
		nextPutAll: newSelector;
		nextPutAll: ' permutation: '.
	permutation storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> superPreconditions [

	| conditions |
	
	conditions := self myConditions
	              & (RBDefinesSelectorsCondition new definesSelector: {oldSelector} in: class)
	              & (ReBlockCondition new 
							block: [ newSelector asString isValidSelector ];
							violatorErrorString: newSelector ,' is not a valid selector').

	^ conditions & self doesNotOverrideExistingMethodPreconditions


]

{ #category : 'accessing' }
ReRenameMethodRefactoring >> violations [ 

	^ violations ifNil: [ violations := OrderedCollection new ]
]
