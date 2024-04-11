"
I exist to test refactoring conditions
"
Class {
	#name : 'MyClassAlpha',
	#superclass : 'Object',
	#instVars : [
		'instVarA'
	],
	#classVars : [
		'SharedVarA'
	],
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'dummy methods' }
MyClassAlpha >> methodWithSuperSend [
	" I exist to give MyClassB>>methodWithSuperSend sense "
	^ 24
]
