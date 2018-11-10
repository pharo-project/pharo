"
I represent query browser state which includes active scope and active query and extra scopes list. 
 
Internal Representation and Key Implementation Points.

    Instance Variables
	activeScope:		<Object>
	methodQuery:		<Object>
	queryScopes:		<Object>
"
Class {
	#name : #ClyQueryBrowserState,
	#superclass : #ClyBrowserState,
	#instVars : [
		'activeScope',
		'methodQuery',
		'queryScopes'
	],
	#category : #'Calypso-SystemTools-QueryBrowser-UI'
}

{ #category : #accessing }
ClyQueryBrowserState >> activeScope [
	^ activeScope
]

{ #category : #accessing }
ClyQueryBrowserState >> activeScope: anObject [
	activeScope := anObject
]

{ #category : #private }
ClyQueryBrowserState >> createBrowser [
	^ClyQueryBrowser new
]

{ #category : #accessing }
ClyQueryBrowserState >> queryScopes [
	^ queryScopes
]

{ #category : #accessing }
ClyQueryBrowserState >> queryScopes: anObject [
	queryScopes := anObject
]

{ #category : #private }
ClyQueryBrowserState >> restoreStateOf: aBrowser [
	super restoreStateOf: aBrowser.
	
	aBrowser  
		systemQuery: methodQuery;
		queryScopes: queryScopes;
		activeScope: activeScope
]

{ #category : #operations }
ClyQueryBrowserState >> retrieveStateFrom: aBrowser [
	super retrieveStateFrom: aBrowser.
	
	queryScopes := aBrowser queryScopes.
	activeScope := aBrowser activeScope.
	methodQuery := aBrowser systemQuery
]

{ #category : #accessing }
ClyQueryBrowserState >> systemQuery [
	^ methodQuery
]

{ #category : #accessing }
ClyQueryBrowserState >> systemQuery: anObject [
	methodQuery := anObject
]
