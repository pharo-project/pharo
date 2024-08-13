Class {
	#name : 'RBRenameMethodTransformation',
	#superclass : 'RBChangeMethodNameTransformation',
	#instVars : [
		'hasPermutedArguments'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'instance creation' }
RBRenameMethodTransformation class >> model: aRBSmalltalk renameMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^ self new
		model: aRBSmalltalk;
		renameMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
]

{ #category : 'instance creation' }
RBRenameMethodTransformation class >> renameMethod: aSelector in: aClass to: newSelector permutation: aMap [
	^ self new
		renameMethod: aSelector
		in: aClass
		to: newSelector
		permutation: aMap
]

{ #category : 'preconditions' }
RBRenameMethodTransformation >> applicabilityPreconditions [
	"Check skipping preconditions first, if they fail, check applicability preconditions."

	^ super applicabilityPreconditions , {
		  (
			(RBCondition
			   withBlock: [ newSelector ~= oldSelector ]
			   errorString:
			   'The selector name has <1?not:> changed <1?:to #' , newSelector
			   , '>')
			|
		  (RBCondition
			   withBlock: [
			   permutation asArray ~= (1 to: oldSelector numArgs) asArray ]
			   errorString: 'The arguments are <1?:not >permuted')).
		  (RBCondition
			   withBlock: [ oldSelector numArgs = newSelector numArgs ]
			   errorString: newSelector printString
				   , ' doesn''t have the correct number of arguments.') }
]

{ #category : 'accessing' }
RBRenameMethodTransformation >> hasPermutedArguments [

	^ hasPermutedArguments
		ifNil: [ hasPermutedArguments := super hasPermutedArguments ]
		ifNotNil: [ hasPermutedArguments ]
]

{ #category : 'accessing' }
RBRenameMethodTransformation >> implementorsCanBePrimitives [
	^self hasPermutedArguments not
]

{ #category : 'accessing' }
RBRenameMethodTransformation >> modifyImplementorParseTree: parseTree in: aClass [
	super modifyImplementorParseTree: parseTree in: aClass.
	self renameArgumentsIn: parseTree
]

{ #category : 'accessing' }
RBRenameMethodTransformation >> parseTreeRewriter [
	| rewriteRule oldString newString |
	oldString := self buildSelectorString: oldSelector.
	newString := self buildSelectorString: newSelector
				withPermuteMap: permutation.
	rewriteRule := self hasPermutedArguments
				ifTrue: [self parseTreeRewriterClass new]
				ifFalse: [self parseTreeRewriterClass replaceLiteral: oldSelector with: newSelector].
	rewriteRule replace: '``@object ' , oldString
		with: '``@object ' , newString.
	^rewriteRule
]

{ #category : 'storing' }
RBRenameMethodTransformation >> storeOn: aStream [
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
