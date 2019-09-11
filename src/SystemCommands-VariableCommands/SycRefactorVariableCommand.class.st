"
I am a base class for command which perform particular kind refactoring on given variables.

I provide suitable method to create refactoring instances: 

- createRefactorings: variableRefactoringClass
- createRefactorings: variableRefactoringClass using: initBlock

Subclasses use this methods to create refactoring instances.

I delegate actual refactoring instantiation to the each variable. It allows abstract difference between different kind of variables and to have single command for same kind of class variable refactoring and instance variable refactoring:

	eachVar createVariableRefactoring: variableRefactoringClass
	
Look at #createVariableRefactoring: implementors for details.
"
Class {
	#name : #SycRefactorVariableCommand,
	#superclass : #SycVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #testing }
SycRefactorVariableCommand class >> canBeExecutedInContext: aToolContext [
	^(super canBeExecutedInContext: aToolContext)
		and: [ aToolContext isGlobalVariableSelected not ]
]

{ #category : #testing }
SycRefactorVariableCommand class >> isAbstract [
	^self = SycRefactorVariableCommand 
]

{ #category : #execution }
SycRefactorVariableCommand >> createRefactorings: variableRefactoringClass [
	^self createRefactorings: variableRefactoringClass using: [ ]
]

{ #category : #execution }
SycRefactorVariableCommand >> createRefactorings: variableRefactoringClass using: initBlock [
	| refactoring |
	^variables collect: [ :eachVar |
		refactoring := eachVar createVariableRefactoring: variableRefactoringClass.
		initBlock cull: refactoring cull: eachVar.
		refactoring]
]

{ #category : #execution }
SycRefactorVariableCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycRefactorVariableCommand >> isComplexRefactoring [
	^true
]
