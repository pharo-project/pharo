Class {
	#name : #SycProtectVariableCommand,
	#superclass : #SycRefactorVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #execution }
SycProtectVariableCommand >> asRefactorings [

	^self 
		createRefactorings: RBProtectInstanceVariableRefactoring 
		using: [ :refactoring :var |
			refactoring variable: var name class: var definingClass ]
]

{ #category : #accessing }
SycProtectVariableCommand >> defaultMenuIconName [ 
	^ #group
]

{ #category : #accessing }
SycProtectVariableCommand >> defaultMenuItemName [
	^'Protect'
]
