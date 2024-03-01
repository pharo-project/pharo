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

	^ { 
		self validSelectorCondition .
		self methodDefinedInClassCondition . 
		self newNameDoesNotRequireRefactoringPrecondition not . 
		self selectorsHaveSameArity }
]

{ #category : 'torevisit' }
ReRenameMethodRefactoring >> areArgumentsPermuted [

	^ permutation asArray ~= (1 to: oldSelector numArgs) asArray
]

{ #category : 'testing' }
ReRenameMethodRefactoring >> areNamesTheSame [

	^ newSelector = oldSelector
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> breakingChangePreconditions [

	^ { ReUpToRootDoesNotDefinesMethod new
		  classes: self implementors;
		  selector: newSelector }
]

{ #category : 'redefined locally waiting for superclass migration' }
ReRenameMethodRefactoring >> generateChanges [
	"Prepare, check the preconditions, and perform the actual transformation (i.e., creating a list of changes that can be accessible using the changes message). This method should not be invoked from drivers since drivers usually check in finer grained mode the preconditions - and that this stage they already checked the preconditions."
		
	"since we locally redefine generate changes we do not need anymore preconditions.
	We should probably reintroduce it in the future as preconditions 
	^ self applicabilityPreconditions addAll: self breakingChangesPreconditions but it will be once the complete hierarchy is
	migrated to the new way."	

	self prepareForExecution.
	
	"this is a bit bad since on error we execute twice the preconditions
	once to check and one to report. to be fixed later"
	
	self failedApplicabilityPreconditions ifNotEmpty: [
		^ RBApplicabilityChecksFailedError signal: 'Prpblems with selector'
			  "should fix this later self applicabilityPreconditions errorString" ].
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
ReRenameMethodRefactoring >> methodDefinedInClassCondition [

	^ (ReDefinesSelectorsCondition new definesSelectors: {oldSelector} in: class)
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

{ #category : 'accessing' }
ReRenameMethodRefactoring >> refactoredClass [
	^ class
]

{ #category : 'interactive' }
ReRenameMethodRefactoring >> renameChanges [

	self privateTransform.
	^ self changes
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> selectorsHaveSameArity [
	"For a rename we only check for the same arity, in add parameter obviously it should be different"
	
	^ ReBlockCondition new 
		block: [ self haveSelectorsSameArity ];
		violatorErrorString: newSelector printString
				, ' doesn''t have the same number of arguments as ', oldSelector printString
]

{ #category : 'storing' }
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
ReRenameMethodRefactoring >> validSelectorCondition [

	^ (ReBlockCondition new 
			block: [ newSelector asString isValidSelector ];
			violatorErrorString: newSelector ,' is not a valid selector').
]

{ #category : 'accessing' }
ReRenameMethodRefactoring >> violations [ 

	^ self breakingChangePreconditions violators
]
