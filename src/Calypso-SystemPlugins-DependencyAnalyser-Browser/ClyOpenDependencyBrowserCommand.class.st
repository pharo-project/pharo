"
I am a command to open dependency browser on set of packages
"
Class {
	#name : 'ClyOpenDependencyBrowserCommand',
	#superclass : 'SycPackageCommand',
	#category : 'Calypso-SystemPlugins-DependencyAnalyser-Browser',
	#package : 'Calypso-SystemPlugins-DependencyAnalyser-Browser'
}

{ #category : 'execution' }
ClyOpenDependencyBrowserCommand >> executeRefactoring [

	(StPackageDependenciesPresenter onPackagesNamed: (packages collect: [ :package | package name ])) open
]

{ #category : 'accessing' }
ClyOpenDependencyBrowserCommand >> menuGroup [

	^ ClyQueryMenuGroup
]

{ #category : 'accessing' }
ClyOpenDependencyBrowserCommand >> name [
	^'Browse dependencies'
]

{ #category : 'accessing' }
ClyOpenDependencyBrowserCommand >> order [

	^ 1000
]
