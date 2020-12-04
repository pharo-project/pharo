Class {
	#name : #StInspectorHelpCommand,
	#superclass : #StInspectorCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #default }
StInspectorHelpCommand class >> defaultDescription [

	^ 'Open help browser'
]

{ #category : #initialization }
StInspectorHelpCommand class >> defaultIconName [

	^ #glamorousHelp
]

{ #category : #default }
StInspectorHelpCommand class >> defaultName [

	^ 'Help'
]

{ #category : #execution }
StInspectorHelpCommand >> execute [ 

	self flag: #TODO.
	"HelpBrowser open selectTopicSatisfying: [:each |
		each owner notNil 
			and: [ (each owner key = GTInspectorHelp key) 
			and: [ each title = 'Overview' ] ] ]"
]
