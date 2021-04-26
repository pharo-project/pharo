Class {
	#name : #ClyBrowserQueryExampleResult,
	#superclass : #ClyBrowserQueryResult,
	#category : #'Calypso-NavigationModel-Tests'
}

{ #category : #building }
ClyBrowserQueryExampleResult >> fillWith: objects [
	items := objects collect: [ :each | ClyBrowserItem named: each printString with: each ]
]
