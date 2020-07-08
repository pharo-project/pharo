Class {
	#name : #StInspectorHelpCommand,
	#superclass : #StInspectorCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #default }
StInspectorHelpCommand class >> defaultDescription [

	^ 'Open help browser'
]

{ #category : #default }
StInspectorHelpCommand class >> defaultName [

	^ 'Help'
]

{ #category : #converting }
StInspectorHelpCommand >> asSpecCommand [
	
	^ super asSpecCommand
		iconName: #glamorousHelp;
		yourself
]

{ #category : #execution }
StInspectorHelpCommand >> execute [ 

	HelpBrowser open selectTopicSatisfying: [:each |
		each owner notNil 
			and: [ (each owner key = GTInspectorHelp key) 
			and: [ each title = 'Overview' ] ] ]
]
