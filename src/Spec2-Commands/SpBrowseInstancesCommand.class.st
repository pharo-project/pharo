"
I browse all instances of the class returned by the evaluation of my context
"
Class {
	#name : #SpBrowseInstancesCommand,
	#superclass : #SpSystemDiscoveryCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #default }
SpBrowseInstancesCommand class >> defaultDescription [
	^ 'Browse all instances of the selected class'
]

{ #category : #accessing }
SpBrowseInstancesCommand class >> shortName [

	^ 'all instances'
]

{ #category : #executing }
SpBrowseInstancesCommand >> execute [

	self target instanceSide inspectAllInstances
]
