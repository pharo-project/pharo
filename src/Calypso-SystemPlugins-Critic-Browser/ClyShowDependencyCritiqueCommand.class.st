"
I a command which opens dependency browser of given package which is my analyzed object
"
Class {
	#name : #ClyShowDependencyCritiqueCommand,
	#superclass : #ClyCritiqueCommand,
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #execution }
ClyShowDependencyCritiqueCommand class >> canBeExecutedInContext: aCriticContext [
	(super canBeExecutedInContext: aCriticContext) ifFalse: [ ^false ].
	
	^aCriticContext selectedCritique class = ArchitecturalCritique
]

{ #category : #accessing }
ClyShowDependencyCritiqueCommand >> defaultMenuIconName [
	^#smallWindow
]

{ #category : #'table decoration' }
ClyShowDependencyCritiqueCommand >> description [
	^'Open dependency analyzer'
]

{ #category : #execution }
ClyShowDependencyCritiqueCommand >> execute [

	(DAPackageAnalyzerPackageDependenciesWindow
		onPackagesNamed: {analyzedObject packageName}) open
]
