"
I a command which opens dependency browser of given package which is my analyzed object
"
Class {
	#name : 'ClyShowDependencyCritiqueCommand',
	#superclass : 'ClyCritiqueCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'testing' }
ClyShowDependencyCritiqueCommand class >> canBeExecutedInContext: aCriticContext [
	(super canBeExecutedInContext: aCriticContext) ifFalse: [ ^false ].

	^aCriticContext selectedCritique class = ArchitecturalCritique
]

{ #category : 'table decoration' }
ClyShowDependencyCritiqueCommand >> description [
	^'Open dependency analyzer'
]

{ #category : 'execution' }
ClyShowDependencyCritiqueCommand >> executeRefactoring [

	(StPackageDependenciesPresenter onPackagesNamed: { self analyzedObject name }) open
]

{ #category : 'accessing' }
ClyShowDependencyCritiqueCommand >> iconName [
	^#smallWindow
]
