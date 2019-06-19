"
I am a command to open dependency browser on set of packages
"
Class {
	#name : #ClyOpenDependencyBrowserCommand,
	#superclass : #SycPackageCommand,
	#category : #'Calypso-SystemPlugins-DependencyAnalyser-Browser'
}

{ #category : #activation }
ClyOpenDependencyBrowserCommand class >> packageContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: RPackage asCalypsoItemContext 
]

{ #category : #accessing }
ClyOpenDependencyBrowserCommand >> defaultMenuItemName [
	^'Browse dependencies'
]

{ #category : #execution }
ClyOpenDependencyBrowserCommand >> execute [

	| packageNames |
	packageNames := packages collect: [ :each | each name ].
	
	(DAPackageAnalyzerPackageDependenciesWindow onPackagesNamed: packageNames) open 
]
