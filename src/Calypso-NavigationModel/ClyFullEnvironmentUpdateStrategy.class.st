"
I am special strategy to perform full update of environment after multiple changes when all cached query results were correctly updated. 
I collect all changes and publish them all together when environment finish overall processing.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	changes:		<IdentityDictionary of<ClyQueryResult, Announcement>>
"
Class {
	#name : #ClyFullEnvironmentUpdateStrategy,
	#superclass : #ClyEnvironmentUpdateStrategy,
	#instVars : [
		'changes'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #updating }
ClyFullEnvironmentUpdateStrategy >> announceChangesOf: aQueryResult [

	changes add: aQueryResult
]

{ #category : #initialization }
ClyFullEnvironmentUpdateStrategy >> initialize [
	super initialize.
	
	changes := IdentitySet new
]

{ #category : #updating }
ClyFullEnvironmentUpdateStrategy >> publishCollectedChanges [
	changes do: [ :queryResult | queryResult notifyChanges ].
	changes removeAll
]
