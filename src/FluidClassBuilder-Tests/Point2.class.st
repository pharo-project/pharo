Class {
	#name : #Point2,
	#superclass : #Object,
	#instVars : [
		'uniqueInstance',
		'u',
		'y'
	],
	#classInstVars : [
		'uniqueInstance',
		'u'
	],
	#category : #'FluidClassBuilder-Tests-Base'
}

{ #category : #accessing }
Point2 class >> uniqueInstance [ 

	^ uniqueInstance 
]
