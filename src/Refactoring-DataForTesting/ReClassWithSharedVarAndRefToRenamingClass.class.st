Class {
	#name : 'ReClassWithSharedVarAndRefToRenamingClass',
	#superclass : 'Object',
	#classVars : [
		'SharedVariableWithRef'
	],
	#category : 'Refactoring-DataForTesting-ForDuplication',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForDuplication'
}

{ #category : 'methods for tests' }
ReClassWithSharedVarAndRefToRenamingClass >> dummyMethod [

	^ ReClassToBeRenamed new + SharedVariableWithRef 
]
