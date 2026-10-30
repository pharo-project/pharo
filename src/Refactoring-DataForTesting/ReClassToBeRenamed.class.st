Class {
	#name : 'ReClassToBeRenamed',
	#superclass : 'Object',
	#classVars : [
		'SharedVar'
	],
	#category : 'Refactoring-DataForTesting-ForDuplication',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForDuplication'
}

{ #category : 'actions' }
ReClassToBeRenamed >> checkBehaviorPrecondition [

	ReClassToBeRenamedChild new 
]

{ #category : 'initialization' }
ReClassToBeRenamed >> initialize [

	<ignoreUnusedClassVariables: #( #SharedVar )>
	super initialize
]
