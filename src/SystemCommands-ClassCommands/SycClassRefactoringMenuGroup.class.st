"
I am the ""Refactorings"" sub-menu of the context menu of classes. Meta+T and the tab action of class editors open my commands as a pop-up menu (ClyOpenMenuGroupCommand).
"
Class {
	#name : 'SycClassRefactoringMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'accessing' }
SycClassRefactoringMenuGroup >> description [

	^ 'Opens special class menu.'
]

{ #category : 'accessing' }
SycClassRefactoringMenuGroup >> name [

	^ 'Refactorings'
]

{ #category : 'accessing' }
SycClassRefactoringMenuGroup >> order [

	^ 10100
]
