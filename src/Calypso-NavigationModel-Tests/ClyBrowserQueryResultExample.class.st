Class {
	#name : #ClyBrowserQueryResultExample,
	#superclass : #ClyBrowserQueryResult,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #building }
ClyBrowserQueryResultExample >> fillWith: objects [
	items := objects collect: [ :each | ClyBrowserItem named: each printString with: each ]
]
