"
Open dependency analyzer

I a command which opens dependency browser of given package which is my analyzed object
"
Class {
	#name : 'ClyShowDependencyCritiqueCommand',
	#superclass : 'ClyCritiqueCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'testing' }
ClyShowDependencyCritiqueCommand >> canBeExecuted [

	super canBeExecuted ifFalse: [ ^ false ].

	^ self context selectedCritique class = ArchitecturalCritique
]

{ #category : 'execution' }
ClyShowDependencyCritiqueCommand >> executeRefactoring [

	(StPackageDependenciesPresenter onPackagesNamed: { self analyzedObject name }) open
]

{ #category : 'accessing' }
ClyShowDependencyCritiqueCommand >> iconName [
	^#smallWindow
]
