"
I am a scope of projects.
So I see all objects which are defined in given projects:
- projects
- packages
- classes 
- methods
"
Class {
	#name : #ClyProjectScope,
	#superclass : #ClyTypedScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #accessing }
ClyProjectScope class >> defaultName [
	^'projects'
]

{ #category : #queries }
ClyProjectScope >> classesDo: aBlock [
	self projectsDo: [ :project |
		project classes do: aBlock ]
]

{ #category : #'system changes' }
ClyProjectScope >> includesClassesAffectedBy: aSystemAnnouncement [
	^false
]

{ #category : #queries }
ClyProjectScope >> packagesDo: aBlock [
	self projectsDo: [ :project |
		project packages do: aBlock ]
]

{ #category : #queries }
ClyProjectScope >> projectsDo: aBlock [
	basisObjects do: aBlock
]
