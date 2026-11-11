"
I run all tests from selected packages
"
Class {
	#name : 'ClyRunTestsFromPackagesCommand',
	#superclass : 'ClyRunTestsForPackageOrClassGroupCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'activation' }
ClyRunTestsFromPackagesCommand class >> contextClass [
	^Package asCalypsoItemContext
]

{ #category : 'accessing' }
ClyRunTestsFromPackagesCommand >> description [

	^ 'Run all tests from selected packages.'
]
