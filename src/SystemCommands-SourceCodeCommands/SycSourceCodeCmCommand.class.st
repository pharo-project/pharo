"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : #SycSourceCodeCmCommand,
	#superclass : #SycCmCommand,
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #'as yet unclassified' }
SycSourceCodeCmCommand class >> activationStrategy [ 
	^ SycSourceCodeMenuActivation
]

{ #category : #accessing }
SycSourceCodeCmCommand >> order [
	
	^ 100
]
