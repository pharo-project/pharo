"
An AbstractNautilusUITest is a test class for testing the behavior of AbstractNautilusUI
"
Class {
	#name : #AbstractNautilusUITest,
	#superclass : #TestCase,
	#instVars : [
		'model'
	],
	#category : #Nautilus-Tests
}

{ #category : #running }
AbstractNautilusUITest >> setUp [
	"Setting up code for AbstractNautilusUITest"

	model := AbstractNautilusUI new.
]

{ #category : #tests }
AbstractNautilusUITest >> testSortClassesInCachedHierarchyB [
]
