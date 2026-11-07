"
I am a shared pool used for testing.
"
Class {
	#name : 'TestSharedPool',
	#superclass : 'SharedPool',
	#classVars : [
		'One',
		'ReferencedInTest'
	],
	#category : 'Kernel-CodeModel-Tests-Support',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Support'
}

{ #category : 'class initialization' }
TestSharedPool class >> initialize [
	One := 1
]
