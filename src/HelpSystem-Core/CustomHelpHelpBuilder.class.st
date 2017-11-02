"
This builder builds help topics from a help topic description (which is typically stored
in a class). 

The help topic description object has to understand the following messages:

  #bookName - should return the name of the help book
  #icon - should return the icon of the help book
  #key - should return a unique key to identify the book
  #pages - should return an array of method selectors to call to get the books pages

"
Class {
	#name : #CustomHelpHelpBuilder,
	#superclass : #HelpBuilder,
	#category : 'HelpSystem-Core-Builders'
}

{ #category : #building }
CustomHelpHelpBuilder >> build [
	"Start building a help topic from a code description"
	
	topicToBuild := self createTopicFrom: rootToBuildFrom 

]

{ #category : #private }
CustomHelpHelpBuilder >> createTopicFrom: aDescription [
	"Create a topic from a description stored on a class.
	aDescription can specify (via #pages) the name of a class and not
	only a selector. This allows for hierarchies with 'subtrees in the middle'"
	
	|topic page   pageClasses |
	topic := HelpTopic named: aDescription bookName.
	topic contents: aDescription bookDescription.
	topic key: aDescription key.
	topic icon: aDescription icon.
	pageClasses := Set new.
	aDescription pages do: [:pageSelectorOrClass|
		page:= (Smalltalk hasClassNamed: pageSelectorOrClass asString)
					ifFalse: [aDescription perform: pageSelectorOrClass]
					ifTrue: [pageClasses add: (Smalltalk classNamed: pageSelectorOrClass asString).
							 (Smalltalk classNamed: pageSelectorOrClass asString) asHelpTopic].
		topic addSubtopic: page.
	].	
	((aDescription subclasses asSet)
		removeAllFoundIn: pageClasses;
		yourself) do: [:subclass | topic subtopics add: subclass asHelpTopic ].
	^topic
]
