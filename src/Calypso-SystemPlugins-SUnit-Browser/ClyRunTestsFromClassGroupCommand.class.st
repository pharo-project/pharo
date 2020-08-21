"
I run all tests from selected classGroup (package tags)
"
Class {
	#name : #ClyRunTestsFromClassGroupCommand,
	#superclass : #ClyRunTestsForPackageOrClassGroupCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #activation }
ClyRunTestsFromClassGroupCommand class >> contextClass [
	^ ClyClassGroup asCalypsoItemContext
]
