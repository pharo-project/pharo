"
I am a refactoring operation for renaming methods.

The new method name has to have the same number of arguments, but the order of arguments can be changed.

My preconditions verify that the number of arguments is the same and that the new method name isn't already used.

All references in senders of the old method are changed, either the method name only or the order of the supplied arguments.

"
Class {
	#name : #RBRenameMethodRefactoring,
	#superclass : #RBChangeMethodNameRefactoring,
	#instVars : [
		'hasPermutedArguments'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRenameMethodRefactoring class >> model: aRBSmalltalk renameMethod: aSelector in: aClass to: newSelector permutation: aMap [ 
	^(self new)
		model: aRBSmalltalk;
		renameMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
]

{ #category : #'instance creation' }
RBRenameMethodRefactoring class >> renameMethod: aSelector in: aClass to: newSelector permutation: aMap [ 
	^self new renameMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
]

{ #category : #testing }
RBRenameMethodRefactoring >> hasPermutedArguments [

	^ hasPermutedArguments
		ifNil: [ hasPermutedArguments := super hasPermutedArguments ]
		ifNotNil: [ hasPermutedArguments ]
]

{ #category : #testing }
RBRenameMethodRefactoring >> implementorsCanBePrimitives [
	^self hasPermutedArguments not
]

{ #category : #preconditions }
RBRenameMethodRefactoring >> myConditions [
	^RBCondition withBlock: [oldSelector numArgs = newSelector numArgs]
		errorString: newSelector printString 
				, ' doesn''t have the correct number of arguments.'
]

{ #category : #transforming }
RBRenameMethodRefactoring >> parseTreeRewriter [
	| rewriteRule oldString newString |
	oldString := self buildSelectorString: oldSelector.
	newString := self buildSelectorString: newSelector
				withPermuteMap: permutation.
	rewriteRule := self hasPermutedArguments 
				ifTrue: [RBParseTreeRewriter new]
				ifFalse: [RBParseTreeRewriter replaceLiteral: oldSelector with: newSelector].
	rewriteRule replace: '``@object ' , oldString
		with: '``@object ' , newString.
	^rewriteRule
]

{ #category : #preconditions }
RBRenameMethodRefactoring >> preconditions [
	| newCondition |
	newCondition := (RBCondition 
							withBlock: [ newSelector = oldSelector ]
							errorString: 'The selector name has <1?not:> changed <1?:to #', newSelector, '>') 
						& (RBCondition 
							withBlock: [permutation asArray ~= (1 to: oldSelector numArgs) asArray]
							errorString: 'The arguments are <1?:not >permuted').
	^newCondition | super preconditions
]

{ #category : #printing }
RBRenameMethodRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' renameMethod: #';
		nextPutAll: oldSelector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' to: #';
		nextPutAll: newSelector;
		nextPutAll: ' permutation: '.
	permutation storeOn: aStream.
	aStream nextPut: $)
]
