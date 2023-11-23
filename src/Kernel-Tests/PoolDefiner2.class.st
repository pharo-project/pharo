"
A pool used for testing purposes
"
Class {
	#name : 'PoolDefiner2',
	#superclass : 'SharedPool',
	#classVars : [
		'AnAuthor',
		'VariableInPoolDefiner2'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'class initialization' }
PoolDefiner2 class >> initialize [

	VariableInPoolDefiner2 := 33.
	AnAuthor := 'NotDucasse'
]
