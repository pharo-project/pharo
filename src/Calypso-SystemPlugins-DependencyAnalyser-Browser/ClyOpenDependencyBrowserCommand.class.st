"
I am a command to open dependency browser on set of packages
"
Class {
	#name : 'ClyOpenDependencyBrowserCommand',
	#superclass : 'SycPackageCommand',
	#category : 'Calypso-SystemPlugins-DependencyAnalyser-Browser',
	#package : 'Calypso-SystemPlugins-DependencyAnalyser-Browser'
}

{ #category : 'activation' }
ClyOpenDependencyBrowserCommand class >> packageContextMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: Package asCalypsoItemContext
]

{ #category : 'accessing' }
ClyOpenDependencyBrowserCommand >> defaultMenuItemName [
	^'Browse dependencies'
]

{ #category : 'execution' }
ClyOpenDependencyBrowserCommand >> execute [

	| packageNames |
	packageNames := packages collect: [ :each | each name ].
	(DAPackageDependenciesPresenter onPackagesNamed: packageNames) open
]
