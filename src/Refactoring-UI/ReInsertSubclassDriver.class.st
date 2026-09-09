"
Given a class I insert a class betweeb that class and its subclasses.

Note that the argument of the class side method is the actual class below which the new subclass should be inserted.
"
Class {
	#name : 'ReInsertSubclassDriver',
	#superclass : 'ReClassInteractionDriver',
	#instVars : [
		'superclass',
		'subclass',
		'comment',
		'beSibling'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReInsertSubclassDriver class >> driverName [

	^ 'Insert new subclass'
]

{ #category : 'accessing - class hierarchy' }
ReInsertSubclassDriver class >> superclass: aClass [
	"superclass is the class that acts as a reference. A new subclass will be inserted below"

	^ self basicNew
		superclass: aClass;
		initialize;
		yourself
]

{ #category : 'accessing - class hierarchy' }
ReInsertSubclassDriver class >> superclass: aClass packageNames: packages [
	"Answer a new instance using aClass as the superclass of the receiver's subclass, and packages as a <Collection> of <String> specifying the package names where the new subclass will be created"

	^ self basicNew
		superclass: aClass;
		packageNames: packages;
		initialize;
		yourself
]

{ #category : 'configuration' }
ReInsertSubclassDriver >> beSibling [

	beSibling := true
]

{ #category : 'accessing' }
ReInsertSubclassDriver >> comment [

	^ comment
]

{ #category : 'accessing' }
ReInsertSubclassDriver >> comment: aString [ 

	comment := aString
]

{ #category : 'initialization' }
ReInsertSubclassDriver >> dialogTitleForClassSelection [

	^ 'Insert subclass of ', superclass name
]

{ #category : 'execution' }
ReInsertSubclassDriver >> gatherUserInput [

	subclass ifNil: [ self requestClass ].
	subclass ifNil: [ ^ false ].
	refactoring className: subclass.
	^ true
]

{ #category : 'initialization' }
ReInsertSubclassDriver >> initialize [

	super initialize.
	beSibling := false
]

{ #category : 'configuration' }
ReInsertSubclassDriver >> instantiateRefactoring [

	^ (RBInsertNewClassRefactoring className: self subclass)
			model: model;
			superclass: self superclass;
			subclasses: self subclasses; 
			packageName: self packageName;
			tagName: self tagName;
			comment: self comment;
			prepareForExecution;
			yourself.
	
]

{ #category : 'accessing' }
ReInsertSubclassDriver >> packageName [
	"Answer a <Symbol> representing the receiver's package name where the new class will be add"

	^ packageName
		ifNil: [ self superclass packageName ] 

]

{ #category : 'initialization' }
ReInsertSubclassDriver >> requestClass [

	(StRefactoringAddClassPresenter newApplication: self application model: self)
		onCancel: [ : dialog | dialog close ];
		onAccept: [ :dialog |
			self
				packageName: dialog presenter packageName;
				tagName: dialog presenter tagName;
				comment: dialog presenter commentPresenterText.
			dialog close ];
		openModal
]

{ #category : 'accessing' }
ReInsertSubclassDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.

	model := self refactoringScopeOn: scopes first.
	superclass := model classObjectFor: superclass

]

{ #category : 'accessing' }
ReInsertSubclassDriver >> subclass [

	^ subclass
]

{ #category : 'accessing' }
ReInsertSubclassDriver >> subclass: aClass [

	subclass := aClass
]

{ #category : 'accessing' }
ReInsertSubclassDriver >> subclasses [
	"the subclasses are the subclasses of the superclass because we want to insert a new class between the superclass and the subclasses"

	^ beSibling
		  ifTrue: [ #(  ) ]
		  ifFalse: [ superclass subclasses ]
]

{ #category : 'accessing' }
ReInsertSubclassDriver >> superclass [

	^ superclass
]

{ #category : 'accessing' }
ReInsertSubclassDriver >> superclass: anObject [

	superclass := anObject
]
