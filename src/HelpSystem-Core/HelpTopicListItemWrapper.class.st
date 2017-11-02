"
This class implements a list item wrapper for help topics.

Instance Variables

"
Class {
	#name : #HelpTopicListItemWrapper,
	#superclass : #PluggableListItemWrapper,
	#category : 'HelpSystem-Core-UI'
}

{ #category : #accessing }
HelpTopicListItemWrapper >> asString [
	"Returns a string used as a label"
	
	^item title
]

{ #category : #accessing }
HelpTopicListItemWrapper >> balloonText [
	"Returns a string used for fly by help"
	
	^self item title
]

{ #category : #accessing }
HelpTopicListItemWrapper >> contents [
	"Returns subnodes (if any)"
	
 	item hasSubtopics ifFalse: [^#()].	
	^(item subtopics) collect: [ :each | 
		HelpTopicListItemWrapper with: each
	].

]

{ #category : #accessing }
HelpTopicListItemWrapper >> icon [
	"Either return the icon for the given topic"

	item icon notNil
		ifTrue: [ ^ item icon ].
	^ item hasSubtopics
		ifTrue: [ self iconNamed: #bookIcon ]
		ifFalse: [ self iconNamed: #pageIcon ]
]
