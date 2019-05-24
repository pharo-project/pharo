"
I represent global scope of full system environment.
I provide view on all packages, classes and methods in the system.

My basis is instance of ClySystemEnvironment which represents global Smalltalk with friends (package organizer, system annoncer and others).

You can access default instance from current image environment: 

	ClyNavigationEnvironment currentImageScope
"
Class {
	#name : #ClySystemEnvironmentScope,
	#superclass : #ClySystemScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #'refactoring support' }
ClySystemEnvironmentScope >> asRBEnvironment [

	^RBBrowserEnvironment new
]

{ #category : #queries }
ClySystemEnvironmentScope >> classesDo: aBlock [
	
	self system classes do: aBlock
]

{ #category : #'system changes' }
ClySystemEnvironmentScope >> includesClassesAffectedBy: aSystemAnnouncement [
	^aSystemAnnouncement affectsClasses
]

{ #category : #'system changes' }
ClySystemEnvironmentScope >> includesMethodsAffectedBy: aSystemAnnouncement [
	^aSystemAnnouncement affectsMethods
]

{ #category : #testing }
ClySystemEnvironmentScope >> isCurrentImage [
	
	^self system == ClySystemEnvironment currentImage
]

{ #category : #queries }
ClySystemEnvironmentScope >> methodsDo: aBlock [
	
	self classesDo: [:class | 
		class instanceSide localMethods do: aBlock.
		class classSide localMethods do: aBlock]
]

{ #category : #queries }
ClySystemEnvironmentScope >> packagesDo: aBlock [
	
	self system packages do: aBlock
]

{ #category : #queries }
ClySystemEnvironmentScope >> processesDo: aBlock [
	
	self system allProcesses do: aBlock
]

{ #category : #'accessing - projects' }
ClySystemEnvironmentScope >> projectQueryFor: aScope [ 
	
	^ environment system projectManager projectQueryFor: aScope 
]

{ #category : #queries }
ClySystemEnvironmentScope >> projectsDo: aBlock [
	
	self system projects do: aBlock
]
