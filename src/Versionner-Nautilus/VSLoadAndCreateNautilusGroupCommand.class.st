"
Load into your image the selected version or baseline. If you already have a version installed, then it will be simply overriden.
After loading, it creates a group in Nautilus with all the loaded packages.
"
Class {
	#name : #VSLoadAndCreateNautilusGroupCommand,
	#superclass : #VSLoadCommand,
	#category : #'Versionner-Nautilus'
}

{ #category : #execute }
VSLoadAndCreateNautilusGroupCommand >> createGroupWithLoadedPackages [
	LoadedPackagesGroupCreatingCodeProvider
		configurationName: workflow project configurationClass asString
		versionString: workflow project version versionString
		projectName: workflow project name
]

{ #category : #execute }
VSLoadAndCreateNautilusGroupCommand >> execute [
	super execute.
	self createGroupWithLoadedPackages
]

{ #category : #accessing }
VSLoadAndCreateNautilusGroupCommand >> title [
	^ 'load version and create a group in Nautilus'
]
