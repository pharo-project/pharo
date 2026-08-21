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
		'hasPermutedArguments'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
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

	^ { self validSelectorCondition . self selectorsHaveSameArity }
]

{ #category : 'torevisit' }
ReRenameMethodRefactoring >> areArgumentsPermuted [

	^ permutation asArray ~= (1 to: oldSelector numArgs) asArray
]

{ #category : 'torevisit' }
ReRenameMethodRefactoring >> areArgumentsRenamed [

	^ renameMap isNotEmpty 
]

{ #category : 'testing' }
ReRenameMethodRefactoring >> areNamesTheSame [

	^ newSelector = oldSelector
]

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> breakingChangePreconditions [

	^ { (ReUpToRootDoesNotDefinesMethod new
		   classes: self implementors;
		   selector: newSelector) }
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
ReRenameMethodRefactoring >> isMethodDefinitionUnchanged [

	^ self areNamesTheSame & self areArgumentsPermuted not & self areArgumentsRenamed not
		   
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

{ #category : 'instance creation' }
ReRenameMethodRefactoring >> newName: aString [ 
	newSelector := aString.
]

{ #category : 'private' }
ReRenameMethodRefactoring >> parseTreeRewriterInstance [

	self flag: #'dead code - double check if it is lost during the refactoring'.
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

{ #category : 'preconditions' }
ReRenameMethodRefactoring >> validSelectorCondition [

	^ (ReBlockCondition new 
			block: [ self isValidSelector: newSelector ];
			violatorErrorString: newSelector ,' is not a valid selector').
]

{ #category : 'accessing' }
ReRenameMethodRefactoring >> violations [

	^ self breakingChangePreconditions flatCollect: [ :each |
		  each violators ]
]
