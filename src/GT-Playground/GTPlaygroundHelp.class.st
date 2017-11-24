"
A utility class containing the help for the GT Playground.
"
Class {
	#name : #GTPlaygroundHelp,
	#superclass : #CustomHelp,
	#category : #'GT-Playground'
}

{ #category : #accessing }
GTPlaygroundHelp class >> bookName [ 
	^'GT Playground'
]

{ #category : #accessing }
GTPlaygroundHelp class >> key [
	^ self name
]

{ #category : #documentation }
GTPlaygroundHelp class >> overview [
	^ HelpTopic 
		title: 'Overview'
		contents: 
'GT Playground provides an interface for scripting and live programming. It is part of Pharo and was developed as part of the Glamorous Toolkit project (http://gt.moosetechnology.org).'
]

{ #category : #accessing }
GTPlaygroundHelp class >> pages [
	^#(overview )
]
