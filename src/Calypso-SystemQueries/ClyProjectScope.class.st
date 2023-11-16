"
I am a scope of projects.
So I see all objects which are defined in given projects:
- projects
- packages
- classes 
- methods
"
Class {
	#name : 'ClyProjectScope',
	#superclass : 'ClyTypedScope',
	#category : 'Calypso-SystemQueries-Scopes',
	#package : 'Calypso-SystemQueries',
	#tag : 'Scopes'
}

{ #category : 'accessing' }
ClyProjectScope class >> defaultName [
	^'projects'
]

{ #category : 'queries' }
ClyProjectScope >> classesDo: aBlock [

	self packagesDo: [ :package |
		(environment system classesInPackage: package) do: aBlock ]
]

{ #category : 'testing' }
ClyProjectScope >> includesClassesAffectedBy: aSystemAnnouncement [

	self classesDo: [ :class | (aSystemAnnouncement affectsClass: class) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'testing' }
ClyProjectScope >> includesMethodsAffectedBy: aSystemAnnouncement [

	self methodsDo: [ :method | (aSystemAnnouncement affectsMethod: method) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'testing' }
ClyProjectScope >> includesPackagesAffectedBy: aSystemAnnouncement [

	self packagesDo: [ :package | (aSystemAnnouncement affectsPackage: package) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'queries' }
ClyProjectScope >> methodsDo: aBlock [

	self packagesDo: [ :package |
		(environment system methodsInPackage: package) do: aBlock ]
]

{ #category : 'queries' }
ClyProjectScope >> packagesDo: aBlock [

	self projectsDo: [ :project |
		(environment system packagesInProject: project) do: aBlock ]
]

{ #category : 'queries' }
ClyProjectScope >> projectsDo: aBlock [
	basisObjects do: aBlock
]
