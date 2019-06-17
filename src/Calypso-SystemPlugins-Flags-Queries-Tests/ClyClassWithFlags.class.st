"
I am example class which has methods with flag
"
Class {
	#name : #ClyClassWithFlags,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-Flags-Queries-Tests'
}

{ #category : #'methods with flags' }
ClyClassWithFlags >> methodWithFlag [

	self flag: #todo
]

{ #category : #'methods with flags' }
ClyClassWithFlags >> methodWithoutFlag [


]
