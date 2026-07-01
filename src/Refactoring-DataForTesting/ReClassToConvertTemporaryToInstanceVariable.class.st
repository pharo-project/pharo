Class {
	#name : 'ReClassToConvertTemporaryToInstanceVariable',
	#superclass : 'Object',
	#instVars : [
		'instVar'
	],
	#category : 'Refactoring-DataForTesting-ForConvertingVariables',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForConvertingVariables'
}

{ #category : 'action' }
ReClassToConvertTemporaryToInstanceVariable >> doAction1 [

	| tmp1 |
	tmp1 := 35.

	^ tmp1
]

{ #category : 'action' }
ReClassToConvertTemporaryToInstanceVariable >> doAction2 [

	| instVar |
	instVar := 3.
	^ instVar
]

{ #category : 'initialization' }
ReClassToConvertTemporaryToInstanceVariable >> initialize [

	<ignoreUnusedVariables: #( #instVar )>
	super initialize
]
