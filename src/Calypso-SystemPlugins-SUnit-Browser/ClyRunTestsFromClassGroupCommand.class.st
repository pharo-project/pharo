"
I run all tests from selected classGroup (package tags)
"
Class {
	#name : 'ClyRunTestsFromClassGroupCommand',
	#superclass : 'ClyRunTestsForPackageOrClassGroupCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'activation' }
ClyRunTestsFromClassGroupCommand class >> contextClass [
	^ ClyClassGroup asCalypsoItemContext
]

{ #category : 'accessing' }
ClyRunTestsFromClassGroupCommand >> description [

	^ 'Run all tests from selected classGroup.'
]
