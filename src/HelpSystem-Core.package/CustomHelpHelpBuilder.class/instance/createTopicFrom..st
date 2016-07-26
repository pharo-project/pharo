private
createTopicFrom: aDescription
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