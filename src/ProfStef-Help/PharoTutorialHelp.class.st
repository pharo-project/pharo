"
HelpSystem book documenting PharoTutorial
"
Class {
	#name : #PharoTutorialHelp,
	#superclass : #CustomHelp,
	#category : #ProfStef-Help
}

{ #category : #accessing }
PharoTutorialHelp class >> bookName [ 
	^ 'Pharo Tutorial'
]

{ #category : #testing }
PharoTutorialHelp class >> canHaveSyntaxHighlighting [
	^ true 
]

{ #category : #pages }
PharoTutorialHelp class >> createATutorial [
	^ HelpTopic 
		title: 'Create a tutorial'
		contents: 'See AbstractTutorial comment: ', AbstractTutorial comment.
]

{ #category : #pages }
PharoTutorialHelp class >> introduction [
	^ HelpTopic 
		title: 'Introduction'
		contents: 'PharoTutorial is a framework to create interactive tutorials'.
]

{ #category : #pages }
PharoTutorialHelp class >> listOfTutorials [
	^ HelpTopic title: 'List of tutorials' contents: AbstractTutorial contentsOfTutorials
]

{ #category : #accessing }
PharoTutorialHelp class >> pages [
	^ #(introduction listOfTutorials createATutorial)
]
