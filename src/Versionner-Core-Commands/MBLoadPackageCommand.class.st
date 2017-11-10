"
Command to load a package in versionner
"
Class {
	#name : #MBLoadPackageCommand,
	#superclass : #MBAbstractPackageCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #documentation }
MBLoadPackageCommand >> documentation [
	^ 'Perform a project load using the specified package or project as the load list.'
]

{ #category : #execute }
MBLoadPackageCommand >> execute [
	| answer |
	answer := self uiManager confirm: 'Are you sure you want to load using ', target packageName.
	answer ifTrue:  [ | version |
		version := target configurationInfo currentVersion.
		version load: version defaultPackageNamesToLoad, { target packageName } ]
]

{ #category : #accessing }
MBLoadPackageCommand >> title [
	^ 'load ', target typeLabel
]
