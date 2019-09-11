"
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, ""I represent a paragraph of text"".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	environment:		<Object>
	name:		<Object>


    Implementation Points
"
Class {
	#name : #ClyRing2Environment,
	#superclass : #Object,
	#instVars : [
		'environment',
		'name',
		'packageOrganizer',
		'projectManager'
	],
	#category : #'Calypso-Ring'
}

{ #category : #'instance creation' }
ClyRing2Environment class >> on: anRGEvironment [

	^ self new 
		environment: anRGEvironment;
		yourself.
]

{ #category : #converting }
ClyRing2Environment >> asGlobalScopeIn: aNavigationEnvironment [

	
	^ClySystemEnvironmentScope of: self in: aNavigationEnvironment named: name
]

{ #category : #binding }
ClyRing2Environment >> bindingOf: aSymbol [
	
	^ environment bindingOf: aSymbol
]

{ #category : #accessing }
ClyRing2Environment >> classes [
	^ self environment ask allClasses
]

{ #category : #initialization }
ClyRing2Environment >> compileANewClassFrom: aString notifying: aController startingFrom: aClass [
	
	| importer |
	
	importer := RGChunkImporter new.
	importer environment: self environment.
	importer fileInFrom: (aString, '!') readStream.
	
	^ aClass
]

{ #category : #initialization }
ClyRing2Environment >> createPackageNamed: aPackageName [

	^ environment ensurePackageNamed: aPackageName
]

{ #category : #initialization }
ClyRing2Environment >> defaultClassCompiler [

	^ self class compiler
]

{ #category : #accessing }
ClyRing2Environment >> environment [
	^ environment
]

{ #category : #accessing }
ClyRing2Environment >> environment: anObject [
	environment := anObject
]

{ #category : #initialization }
ClyRing2Environment >> includesClassNamed: aSymbol [

	^ self environment includesClassNamed: aSymbol
]

{ #category : #initialization }
ClyRing2Environment >> initialize [

	super initialize.
	
	environment := RGEnvironment new.
	projectManager := ClyProjectManagerRegistry new
]

{ #category : #accessing }
ClyRing2Environment >> name [
	^ name ifNil: [ super printString ]
]

{ #category : #accessing }
ClyRing2Environment >> name: anObject [
	name := anObject
]

{ #category : #accessing }
ClyRing2Environment >> packageOrganizer [
	^ packageOrganizer
]

{ #category : #accessing }
ClyRing2Environment >> packageOrganizer: anObject [
	packageOrganizer := anObject
]

{ #category : #initialization }
ClyRing2Environment >> packages [

	^ environment ask packages
]

{ #category : #accessing }
ClyRing2Environment >> projectManager [
	^ projectManager
]

{ #category : #accessing }
ClyRing2Environment >> projectManager: anObject [
	projectManager := anObject
]

{ #category : #initialization }
ClyRing2Environment >> subscribe: anObject [
	self subscribe: anObject for: SystemAnnouncement
]

{ #category : #initialization }
ClyRing2Environment >> subscribe: anObject for: anAnnouncementClass [
	self unsubscribe: anObject.
	environment announcer when: anAnnouncementClass send: #systemChanged: to: anObject
]

{ #category : #initialization }
ClyRing2Environment >> unsubscribe: anObject [
	environment announcer unsubscribe: anObject
]

{ #category : #announcing }
ClyRing2Environment >> when: anAnnouncementClass send: aSelector to: anObject [
	self environment announcer when: anAnnouncementClass send: aSelector to: anObject
]
