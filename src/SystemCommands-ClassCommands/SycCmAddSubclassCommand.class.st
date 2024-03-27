"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : 'SycCmAddSubclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#instVars : [
		'packageName',
		'tagName',
		'comment',
		'cancelled'
	],
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'accessing' }
SycCmAddSubclassCommand >> comment [

	^ comment
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> comment: anObject [

	comment := anObject
]

{ #category : 'executing' }
SycCmAddSubclassCommand >> executeRefactoring [

	(ReAddSubclassDriver new
		scopes: refactoringScopes subclass: self newClassName;
		packageName: self packageName;
		tagName: self tagName;
		comment: self comment) runRefactoring.
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> icon [
	^self iconNamed:#add
]

{ #category : 'initialization' }
SycCmAddSubclassCommand >> initialize [

	super initialize.
	cancelled := false.
]

{ #category : 'testing' }
SycCmAddSubclassCommand >> isApplicable [

	^ context lastSelectedClass isTrait not
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> name [
	^'New subclass'
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> newClassName [

	^ newClassName 
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> newClassName: aString [ 

	newClassName := aString asSymbol
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> order [
	^ 10100.1
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> packageName [

	^ packageName
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> packageName: anObject [

	packageName := anObject
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> packageNames [
	"Answer a <Collection> of <Symbol> of the packages available to the new class"
	
	^ PackageOrganizer default packageNames
]

{ #category : 'preparation' }
SycCmAddSubclassCommand >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.
	(StRefactoringAddClassPresenter on: self)
		onCancel: [ : dialog |
			"CmdCommandAborted signal"
			cancelled := true.
			dialog close ];
		onAccept: [ :dialog | dialog close ];
		openModal
]

{ #category : 'execution' }
SycCmAddSubclassCommand >> processCommandFailure: anException [

	cancelled 
		ifFalse: [ super processCommandFailure: anException ]
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> tagName [

	^ tagName
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> tagName: anObject [

	tagName := anObject
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> targetPackageName [ 
	"Answer a <Symbol> with the name of the package of the target class (this is the class choosen as superclass to the new one to be created)"
	
	^ self targetClass packageName
]
