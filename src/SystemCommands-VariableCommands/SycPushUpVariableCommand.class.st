"
I am a command to push up given variables
"
Class {
	#name : #SycPushUpVariableCommand,
	#superclass : #SycRefactorVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #execution }
SycPushUpVariableCommand >> asRefactorings [

	^self 
		createRefactorings: RBPullUpInstanceVariableRefactoring 
		using: [ :refactoring :var |
			refactoring variable: var name class: var definingClass superclass]
]

{ #category : #accessing }
SycPushUpVariableCommand >> defaultMenuIconName [ 
	^ #up
]

{ #category : #accessing }
SycPushUpVariableCommand >> defaultMenuItemName [
	^'Push up'
]
