"
Renames a class and all the references to this class as well

Usage:
| transformation |
transformation := (RBRenameClassTransformation
				rename: #RBDummyClassToRename
				to: #RBNewDummyClassName)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the class must exist
- the new class name should be a valid name: not a reserved name, or the name of an existing class variable in the hierarchy
"
Class {
	#name : #RBRenameClassTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'newClassName'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBRenameClassTransformation class >> model: aRBModel rename: aClassName to: aNewClassName [ 

	^ self new
		model: aRBModel;
		className: aClassName
		newClassName: aNewClassName;
		yourself
]

{ #category : #api }
RBRenameClassTransformation class >> rename: aClassName to: aNewClassName [

	^ self new
		className: aClassName
		newClassName: aNewClassName
]

{ #category : #api }
RBRenameClassTransformation >> className: aClassName newClassName: aNewClassName [

	self className: aClassName.
	newClassName := aNewClassName
]

{ #category : #preconditions }
RBRenameClassTransformation >> preconditions [
	^ (RBCondition
		withBlock: [ self definingClass notNil and: [ self definingClass isMeta not ] ])
		& (RBCondition isValidClassName: newClassName)
		& (RBCondition isGlobal: newClassName in: self model) not
		&
			(RBCondition
				withBlock: [ | replacer |
					replacer := (self parseTreeRewriterClass
						replaceLiteral: className
						with: newClassName)
						replace: className with: newClassName;
						replaceArgument: newClassName
							withValueFrom: [ :aNode | 
							self
								refactoringError: newClassName , ' already exists within the reference scope' ];
						yourself.
					self model
						allReferencesToClass: self definingClass
						do: [ :method | 
							(method modelClass hierarchyDefinesVariable: newClassName)
								ifTrue: [ self
										refactoringError:
											newClassName , ' is already defined in hierarchy of '
												, method modelClass printString ].
							self
								convertMethod: method selector
								for: method modelClass
								using: replacer ].
					true ])
		|
			(RBCondition
				withBlock: [ self refactoringError: newClassName , ' is not a valid class name' ])
]

{ #category : #executing }
RBRenameClassTransformation >> privateTransform [

	self model 
		renameClass: self definingClass
		to: newClassName
		around: [self renameReferences]
]

{ #category : #private }
RBRenameClassTransformation >> renameReferences [

	| replacer |
	replacer := (RBParseTreeRewriter
				replaceLiteral: className with: newClassName)
				replace: className with: newClassName;
				"replaceArgument: newClassName withValueFrom: 
					[ :aNode | self refactoringError: newClassName , ' already exists within the reference scope'];" "it shouldnt raise an error at the transformation level"
				yourself.
				
	self model
		allReferencesToClass: self definingClass
		do: [ :method | 
			(method modelClass hierarchyDefinesVariable: newClassName) 
				"ifTrue: [self refactoringError: newClassName,
								' is already defined in hierarchy of ', method modelClass printString]."
				ifFalse: [ self 
					convertMethod: method selector
					for: method modelClass
					using: replacer ]]
]

{ #category : #printing }
RBRenameClassTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' className ''';
		nextPutAll: className;
		nextPutAll: ''' newClassName: ''';
		nextPutAll: newClassName.
	aStream nextPut: $)
]
