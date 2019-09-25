"
I am a refactoring for renaming a class.

My preconditions verify, that the old class exists (in  the current namespace) and that the new class name is valid and not yet used as a global variable name 

The refactoring transformation will replace the current class and its definition with the new class name. And update all references in all methods in this namespace, to use the new name. Even the definition for subclasses of the old class will be changed.

Example
---------------

	(RBRenameClassRefactoring rename: RBRenameClassRefactoring to: 'RBRenameClassRefactoring2') execute
"
Class {
	#name : #RBRenameClassRefactoring,
	#superclass : #RBClassRefactoring,
	#instVars : [
		'newName',
		'class'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRenameClassRefactoring class >> model: aRBSmalltalk rename: aClass to: aNewName [ 
	^(self new)
		model: aRBSmalltalk;
		className: aClass name newName: aNewName;
		yourself
]

{ #category : #'instance creation' }
RBRenameClassRefactoring class >> rename: aClass to: aNewName [
	^self new className: aClass name newName: aNewName
]

{ #category : #initialization }
RBRenameClassRefactoring >> className: aName newName: aNewName [ 
	className := aName asSymbol.
	class := self model classNamed: className.
	newName := aNewName asSymbol
]

{ #category : #preconditions }
RBRenameClassRefactoring >> preconditions [
	^((RBCondition withBlock: [class notNil and: [class isMeta not]]) 
			& (RBCondition isValidClassName: newName) 
			& (RBCondition isGlobal: newName in: self model) not) |
			(RBCondition withBlock: [ self refactoringFailure: newName , ' is not a valid class name'])
]

{ #category : #transforming }
RBRenameClassRefactoring >> renameReferences [
	| replacer |
	replacer := (RBParseTreeRewriter replaceLiteral: className with: newName)
				replace: className with: newName;
				replaceArgument: newName
					withValueFrom: 
						[:aNode | 
						self 
							refactoringFailure: newName , ' already exists within the reference scope'];
				yourself.
	self model allReferencesToClass: class
		do: 
			[:method | 
			(method modelClass hierarchyDefinesVariable: newName) 
				ifTrue: 
					[self refactoringFailure: newName , ' is already defined in hierarchy of ' 
								, method modelClass printString].
			self 
				convertMethod: method selector
				for: method modelClass
				using: replacer]
]

{ #category : #printing }
RBRenameClassRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' rename: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' to: #';
		nextPutAll: newName;
		nextPut: $)
]

{ #category : #transforming }
RBRenameClassRefactoring >> transform [
	self model 
		renameClass: class
		to: newName
		around: [self renameReferences]
]
