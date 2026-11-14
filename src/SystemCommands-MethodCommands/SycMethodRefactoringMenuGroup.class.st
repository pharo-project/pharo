"
I am the ""Refactorings"" sub-menu of the context menu of methods. Meta+T opens my commands as a pop-up menu (ClyOpenMenuGroupCommand).
"
Class {
	#name : 'SycMethodRefactoringMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycMethodRefactoringMenuGroup >> description [

	^ 'Opens special method menu.'
]

{ #category : 'accessing' }
SycMethodRefactoringMenuGroup >> name [

	^ 'Refactorings'
]

{ #category : 'accessing' }
SycMethodRefactoringMenuGroup >> order [

	^ 10100
]
