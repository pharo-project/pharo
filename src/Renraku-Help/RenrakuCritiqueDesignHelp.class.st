"
This book contains explanations of the design behind critiques
"
Class {
	#name : #RenrakuCritiqueDesignHelp,
	#superclass : #RenrakuCritiqueHelp,
	#category : #Renraku-Help
}

{ #category : #pages }
RenrakuCritiqueDesignHelp class >> actionRef [

	^ self topicForClass: RePropertyAction 
]

{ #category : #pages }
RenrakuCritiqueDesignHelp class >> actions [

	^ HelpTopic
		title: 'Custom Actions'
		contents: 
'One can override #actions method to return a list of RePropertyAction objects. Tools can use these objects to provide a user with custom actions defined by critiques themselves.'
]

{ #category : #pages }
RenrakuCritiqueDesignHelp class >> autofix [

	^ HelpTopic
		title: 'Automated Fixes'
		contents: 
'A critique has the #providesChange method which returns a boolean value specifying whether the critique can provide a change which will resolve the issue.

The #change method can be used to obtain an object of RBRefactoryChange kind.'
]

{ #category : #accessing }
RenrakuCritiqueDesignHelp class >> bookDescription [

	^ 'This book contains explanations of the design behind critiques'
]

{ #category : #accessing }
RenrakuCritiqueDesignHelp class >> bookName [

	^ 'Critique Design'
]

{ #category : #pages }
RenrakuCritiqueDesignHelp class >> entity [

	^ HelpTopic
		title: 'Entity Reference'
		contents: 
'A critique has a reference to the criticized entity.

This link is established through ReSourceAnchor. A source anchor has a reference to the actual class, method, or other entity that is criticized. An anchor also has a #providesInterval method that returns a boolean indicating if the anchor provides a selection interval to the actual source of the critique. The interval can be accessed through the #interval method.

There are two subclasses of ReSourceAnchor.
ReIntervalSourceAnchor stores the actual interval object which is set during initialization.
ReSearchStringSourceAnchor stores a searchString which will be searched for in the entities source code on demand to find an interval of substring'
]

{ #category : #pages }
RenrakuCritiqueDesignHelp class >> hierarchy [

	^ HelpTopic
		title: 'Class Hierarchy'
		contents: 
'ReAbstractCritique is the root of the critiques hierarchy.'
]

{ #category : #accessing }
RenrakuCritiqueDesignHelp class >> pages [

	^ #(hierarchy rule entity autofix actions actionRef)
]

{ #category : #pages }
RenrakuCritiqueDesignHelp class >> rule [

	^ HelpTopic
		title: 'Rule Reference'
		contents: 
'A critique has a reference to the rule that reported the violation.
The rule''s #name is used as the critique’s #title and the rule''s #rationale is used as the #description of the critique.'
]
