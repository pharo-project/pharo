"
I am default updating strategy for navigation environment. I force query result to notify observers immediatelly when any change was happen
"
Class {
	#name : #ClyInstantEnvironmentUpdateStrategy,
	#superclass : #ClyEnvironmentUpdateStrategy,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #updating }
ClyInstantEnvironmentUpdateStrategy >> announceChangesOf: aQueryResult [

	aQueryResult notifyChanges
]

{ #category : #updating }
ClyInstantEnvironmentUpdateStrategy >> publishCollectedChanges [

]
