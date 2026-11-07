"
I am a command to push up given variables
"
Class {
	#name : 'SycPushUpVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'accessing' }
SycPushUpVariableCommand >> executeRefactoring [

	^ ((RePushUpVariableDriver newApplication: self application)
		   scopes: refactoringScopes
		   variables: (variables collect: [ :v | v actualVariable ])
		   to: variables first definingClass superclass) runRefactoring
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> fullBrowserOrder [

	^ 2000
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> icon [

	^ self iconNamed: #up
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> name [

	^ '(R) Push up'
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> order [

	^ 1.3
]
