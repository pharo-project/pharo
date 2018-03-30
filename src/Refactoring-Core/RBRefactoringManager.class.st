"
I collect refactorings during a refactory execute-step.

All changes are applied to the system and collected by a unique RBRefactoryChangeManager instance. 
You should use the uniqe instance of me.

"
Class {
	#name : #RBRefactoringManager,
	#superclass : #Object,
	#instVars : [
		'refactorings'
	],
	#classVars : [
		'Instance'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRefactoringManager class >> instance [
	^ Instance ifNil: [ Instance := self basicNew initialize ]
]

{ #category : #'instance creation' }
RBRefactoringManager class >> new [
	^ self shouldNotImplement
]

{ #category : #public }
RBRefactoringManager class >> nuke [
	Instance notNil ifTrue: [ Instance release ].
	Instance := nil
]

{ #category : #public }
RBRefactoringManager class >> unload [
	self nuke
]

{ #category : #'public access' }
RBRefactoringManager >> addRefactoring: aRefactoring [ 
	RBRefactoryChangeManager instance performChange: aRefactoring changes.
	refactorings add: aRefactoring class name
]

{ #category : #initialization }
RBRefactoringManager >> initialize [
	refactorings := Bag new
]

{ #category : #printing }
RBRefactoringManager >> printOn: aStream [
	aStream
		nextPutAll: '#		Refactoring';
		newLine;
		nextPutAll: '---		-----------------------------------------------';
		newLine.
	refactorings asSet asSortedCollection
		do: [ :name | 
			aStream
				nextPutAll: (refactorings occurrencesOf: name) printString;
				nextPutAll: '		';
				nextPutAll: name;
				newLine ]
]
