Class {
	#name : #StPlaygroundHelpCommand,
	#superclass : #StPlaygroundCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StPlaygroundHelpCommand class >> defaultDescription [

	^ 'Open help browser'
]

{ #category : #accessing }
StPlaygroundHelpCommand class >> defaultIconName [

	^ #glamorousHelp
]

{ #category : #default }
StPlaygroundHelpCommand class >> defaultName [

	^ 'Help'
]

{ #category : #execution }
StPlaygroundHelpCommand >> execute [ 

	HelpBrowser open selectTopicSatisfying: [:each |
		each owner notNil 
			and: [ (each owner key = GTPlaygroundHelp key) 
			and: [ each title = 'Overview' ] ] ]
]
