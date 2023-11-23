"
TestLocalVariable is a test class using in ProcessSpecificTest.
"
Class {
	#name : 'TestLocalVariable',
	#superclass : 'ProcessLocalVariable',
	#category : 'Kernel-Tests-Processes',
	#package : 'Kernel-Tests',
	#tag : 'Processes'
}

{ #category : 'defaults' }
TestLocalVariable >> default [
	"My default value for a new process is 0."

	^0
]
