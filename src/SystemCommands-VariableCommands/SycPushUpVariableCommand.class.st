"
I am a command to push up given variables
"
Class {
	#name : 'SycPushUpVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'activation' }
SycPushUpVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.3 for: ClySourceCodeContext
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> executeRefactorings [

	^ ((RePushUpVariableDriver newApplication: self application)
		   scopes: refactoringScopes
		   variables: (variables collect: [ :v | v actualVariable ])
		   to: variables first definingClass superclass) runRefactoring
]

{ #category : 'execution' }
SycPushUpVariableCommand >> iconName [
	^ #up
]

{ #category : 'accessing' }
SycPushUpVariableCommand >> name [
	^'(R) Push up'
]
