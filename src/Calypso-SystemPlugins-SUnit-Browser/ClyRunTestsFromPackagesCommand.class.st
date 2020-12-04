"
I run all tests from selected packages
"
Class {
	#name : #ClyRunTestsFromPackagesCommand,
	#superclass : #ClyRunTestsForPackageOrClassGroupCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #activation }
ClyRunTestsFromPackagesCommand class >> contextClass [
	^RPackage asCalypsoItemContext  
]

{ #category : #activation }
ClyRunTestsFromPackagesCommand class >> fullBrowserClassGroupTableIconActivation [
	<classAnnotation>
	
	^ClyTableIconCommandActivation for: ClyFullBrowserClassGroupContext 
]
