"
I am a refactoring for renaming a class.

My preconditions verify, that the old class exists (in  the current namespace) and that the new class name is valid and not yet used as a global variable name 

The refactoring transformation will replace the current class and its definition with the new class name. And update all references in all methods in this namespace, to use the new name. Even the definition for subclasses of the old class will be changed.

Example
---------------

	(RBRenameClassRefactoring rename: 'RBRenameClassRefactoring' to: 'RBRenameClassRefactoring2') execute
"
Class {
	#name : 'RBRenameClassRefactoring',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'newName',
		'class'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'displaying' }
RBRenameClassRefactoring class >> basicMenuItemString [

	^ 'Rename'
]

{ #category : 'instance creation' }
RBRenameClassRefactoring class >> model: aRBNamespace rename: aClass to: aNewName [
	^ self new
		model: aRBNamespace;
		className: aClass newName: aNewName;
		yourself
]

{ #category : 'partial instantiation' }
RBRenameClassRefactoring class >> rename: aClassName [ 
	^ self new
		className: aClassName ; yourself
]

{ #category : 'instance creation' }
RBRenameClassRefactoring class >> rename: aClassName to: aNewName [
	^ self new
		className: aClassName
		newName: aNewName
]

{ #category : 'preconditions' }
RBRenameClassRefactoring >> applicabilityPreconditions [

	^ (RBCondition withBlock: [
		   self doesClassToBeRenamedExist and: [ self isNotMetaclass ] ])
	  & self isValidClassName & self isNotGlobal
]

{ #category : 'initialization' }
RBRenameClassRefactoring >> className: aName [

	className := aName asSymbol.
	class := self model classNamed: className.
	
]

{ #category : 'initialization' }
RBRenameClassRefactoring >> className: aName newName: aNewName [

	self className: aName.
	self newName: aNewName
]

{ #category : 'preconditions' }
RBRenameClassRefactoring >> doesClassToBeRenamedExist [
	^ class isNotNil 
]

{ #category : 'preconditions' }
RBRenameClassRefactoring >> isMetaclass [

	^ class isMeta
]

{ #category : 'preconditions' }
RBRenameClassRefactoring >> isNotGlobal [

	^ (RBCondition isGlobal: newName in: self model) not
]

{ #category : 'preconditions' }
RBRenameClassRefactoring >> isNotMetaclass [

	^ self isMetaclass not
]

{ #category : 'preconditions' }
RBRenameClassRefactoring >> isValidClassName [

	^ RBCondition isValidClassName: newName
]

{ #category : 'initialization' }
RBRenameClassRefactoring >> newName: aNewName [

	newName := aNewName asSymbol
]

{ #category : 'preconditions' }
RBRenameClassRefactoring >> preconditions [

	self prepareForExecution. 
	"We have to check because the setter is actually already doing the className to class transformation."
	^ self applicabilityPreconditions 
]

{ #category : 'preconditions' }
RBRenameClassRefactoring >> prepareForExecution [

	class := self model classNamed: className
]

{ #category : 'transforming' }
RBRenameClassRefactoring >> privateTransform [

	self model
		renameClass: class
		to: newName
		around: [ self renameReferences ]
]

{ #category : 'transforming' }
RBRenameClassRefactoring >> renameReferences [

	| replacer |
	replacer := (self parseTreeRewriterClass
		             replaceLiteral: className
		             with: newName)
		            replace: className with: newName;
		            replaceArgument: newName withValueFrom: [ :aNode |
			            self refactoringError:
					            newName , ' already exists within the reference scope' ];
		            yourself.
	self model allReferencesToClass: class do: [ :method |
		(method modelClass hierarchyDefinesVariable: newName) ifTrue: [
			self refactoringError: newName , ' is already defined in hierarchy of ' , method modelClass printString ].
		(method method
			 ifNil: [ false ]
			 ifNotNil: [ :m | m isFromTrait]) ifFalse: [ self convertMethod: method selector for: method modelClass using: replacer ] ]
]

{ #category : 'printing' }
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
