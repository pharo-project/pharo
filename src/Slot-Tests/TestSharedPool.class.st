"
I am a shared pool used for testing.
"
Class {
	#name : #TestSharedPool,
	#superclass : #SharedPool,
	#classVars : [
		'One'
	],
	#category : #'Slot-Tests-Data'
}

{ #category : #'class initialization' }
TestSharedPool class >> initialize [
	One := 1
]
