"
Given a class I insert a superclass between its superclass and the class.
Note that the class is given with the superclass for now.
"
Class {
	#name : 'ReInsertSuperclassDriver',
	#superclass : 'ReClassInteractionDriver',
	#instVars : [
		'superclass',
		'subclass',
		'comment',
		'superclassParent'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReInsertSuperclassDriver class >> driverName [

	^ 'Insert new superclass'
]

{ #category : 'accessing - class hierarchy' }
ReInsertSuperclassDriver class >> superclass: aClass [
	"superclass is the class that acts as a reference. A new subclass will be inserted above"

	^ self basicNew
		superclass: aClass;
		initialize;
		yourself
]

{ #category : 'instance creation' }
ReInsertSuperclassDriver class >> superclass: aClass packageNames: packages [
	"Answer a new instance using aClass as the superclass of the receiver's subclass, and packages as a <Collection> of <String> specifying the package names where the new subclass will be created"

	^ self basicNew
		superclass: aClass;
		packageNames: packages;
		initialize;
		yourself
]

{ #category : 'instance creation' }
ReInsertSuperclassDriver class >> superclass: aClass superclassParent: aSuperclass packageNames: packages [
	"Answer a new instance using aClass as the superclass of the receiver's subclass, and packages as a <Collection> of <String> specifying the package names where the new subclass will be created"

	^ self basicNew
		  superclass: aClass;
		  superclassParent: aSuperclass;
		  packageNames: packages;
		  initialize;
		  yourself
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> comment [

	^ comment
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> comment: anObject [

	comment := anObject
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> dialogTitleForClassSelection [

	^ 'Insert new superclass of ' , superclass name
]

{ #category : 'execution' }
ReInsertSuperclassDriver >> gatherUserInput [

	subclass ifNil: [ self requestClass ].
	subclass ifNil: [ ^ false ].
	refactoring className: subclass.
	^ true
]

{ #category : 'configuration' }
ReInsertSuperclassDriver >> instantiateRefactoring [
	"self ensureSuperclassInModel."

	^ (RBInsertNewClassRefactoring className: self subclass asString)
		  model: model;
		  superclass: superclassParent;
		  subclasses: { superclass };
		  packageName: self packageName;
		  tagName: self tagName;
		  comment: self comment;
		  yourself
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> packageName [
	"Answer a <Symbol> representing the receiver's package name where the new class will be add"

	^ packageName
		ifNil: [ self superclass packageName ] 

]

{ #category : 'initialization' }
ReInsertSuperclassDriver >> requestClass [

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
ReInsertSuperclassDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.

	model := self refactoringScopeOn: scopes first.
	superclass := model classObjectFor: superclass
	
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> subclass [

	^ subclass
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> subclass: anObject [

	subclass := anObject
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> superclass [

	^ superclass
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> superclass: anObject [

	superclass := anObject
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> superclassParent [

	^ superclassParent
]

{ #category : 'accessing' }
ReInsertSuperclassDriver >> superclassParent: aClass [

	superclassParent := aClass
]
