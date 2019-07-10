"
I provide access to all projects from different kind of package managers.
It can be Cargo manager, Metacello manager, etc.

ClySystemEnvironment instance keeps my instance. To register new package manager in image use following expression: 
	
	ClySystemEnvironment currentImage registerPackageManager: aPackageManager
	
I allow different kind of package managers to be used by Calypso browser to display all available projects.

Internal Representation and Key Implementation Points.

    Instance Variables
	packageManagers:		<Collection of<ClyPackageManager>>
"
Class {
	#name : #ClyProjectManagerRegistry,
	#superclass : #Object,
	#instVars : [
		'projectManagers'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #initialization }
ClyProjectManagerRegistry >> initialize [
	super initialize.
	projectManagers := OrderedCollection new
]

{ #category : #accessing }
ClyProjectManagerRegistry >> projectManagers [
	^ projectManagers
]

{ #category : #accessing }
ClyProjectManagerRegistry >> projectManagers: anObject [
	projectManagers := anObject
]

{ #category : #queries }
ClyProjectManagerRegistry >> projectQueryFor: aScope [

	projectManagers isEmpty ifTrue: [ ^ ClyUnknownQuery instance ].

	^ (projectManagers collect: [ :found | found queryClass sortedFrom: aScope ])
		reduce: [ :a :b | a , b ]
]

{ #category : #accessing }
ClyProjectManagerRegistry >> projects [
	^projectManagers flatCollect: [ :each | each projects ]
]

{ #category : #registration }
ClyProjectManagerRegistry >> registerProjectManager: aPackageManager [
	projectManagers removeAllSuchThat: [ :each | each class = aPackageManager class ].
	projectManagers add: aPackageManager 
]
