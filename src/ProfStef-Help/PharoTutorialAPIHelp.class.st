"
I'm a PharoTutorial which builds a HelpSystem book by collecting comment from classes and method.
"
Class {
	#name : #PharoTutorialAPIHelp,
	#superclass : #PharoTutorialHelp,
	#category : 'ProfStef-Help'
}

{ #category : #accessing }
PharoTutorialAPIHelp class >> bookName [ 
	^ 'Reference'
]

{ #category : #defaults }
PharoTutorialAPIHelp class >> builder [ 
	^ PackageAPIHelpBuilder
]

{ #category : #accessing }
PharoTutorialAPIHelp class >> helpPackages [
	^ #('ProfStef-Core')
]
