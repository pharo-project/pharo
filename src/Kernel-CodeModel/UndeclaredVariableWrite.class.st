"
This error is signaled on write attempt to undeclared variables
"
Class {
	#name : 'UndeclaredVariableWrite',
	#superclass : 'UndeclaredVariableError',
	#instVars : [
		'value'
	],
	#category : 'Kernel-CodeModel-Exceptions',
	#package : 'Kernel-CodeModel',
	#tag : 'Exceptions'
}

{ #category : 'testing' }
UndeclaredVariableWrite >> defaultResumeValue [

	^ value
]

{ #category : 'accessing' }
UndeclaredVariableWrite >> value [

	^ value
]

{ #category : 'accessing' }
UndeclaredVariableWrite >> value: anObject [

	value := anObject
]
