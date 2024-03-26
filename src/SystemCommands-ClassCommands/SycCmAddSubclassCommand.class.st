"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : 'SycCmAddSubclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#classTraits : '{} + TraitedClass',
	#instVars : [
		'packageName',
		'tagName',
		'comment'
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

{ #category : 'preparation' }
SycCmAddSubclassCommand >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.
	(StRefactoringAddClassPresenter on: self)
		onCancel: [ CmdCommandAborted signal ];
		onAccept: [ :dialog | dialog close ];
		openModal
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> tagName [

	^ tagName
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> tagName: anObject [

	tagName := anObject
]
