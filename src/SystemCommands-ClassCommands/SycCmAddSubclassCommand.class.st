"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : 'SycCmAddSubclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'executing' }
SycCmAddSubclassCommand >> executeRefactoring [

	(ReAddSubclassDriver new
		scopes: refactoringScopes subclass: newClassName;
		superclass: targetClass;		
		package: targetClass package;
		tag: targetClass packageTag) runRefactoring.
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
SycCmAddSubclassCommand >> order [
	^ 10100.1
]

{ #category : 'preparation' }
SycCmAddSubclassCommand >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.
	newClassName := StRefactoringAddClassPresenter new 
		onCancel: [ CmdCommandAborted signal ];
		onAccept: [ :dialog | 
			dialog close.
			dialog presenter text ];
		openModal
]
