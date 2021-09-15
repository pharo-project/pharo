Class {
	#name : #SimpleClass,
	#superclass : #Object,
	#instVars : [
		'var'
	],
	#classInstVars : [
		'var'
	],
	#category : #ClassParser
}

{ #category : #'instance creation' }
SimpleClass class >> with: anInteger [ 
	^ self new with: anInteger
]

{ #category : #'instance creation' }
SimpleClass >> = aSimpleClass [
   ^ (var = aSimpleClass var) asBit
]

{ #category : #'instance creation' }
SimpleClass >> var [
   ^ var
]

{ #category : #'instance creation' }
SimpleClass >> with: anInteger [ 
	var := anInteger 
]
