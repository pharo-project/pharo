"
My subclasses implement commands related to the live system discovery and exploration.
"
Class {
	#name : #SpSystemDiscoveryCommand,
	#superclass : #SpSystemNavigationCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #defaults }
SpSystemDiscoveryCommand class >> defaultIconName [
	^ #smallObjects
]
