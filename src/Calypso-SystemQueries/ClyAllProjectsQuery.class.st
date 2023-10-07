"
I am a query of all projects from given scope.
The project managers should return a subclass of myself.

"
Class {
	#name : 'ClyAllProjectsQuery',
	#superclass : 'ClyTypedQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'items type' }
ClyAllProjectsQuery class >> resultItemsType [
	^ClyManagedProject
]
