"
I represent all the processors that are discovered by being my subclasses.
This mechanism is much, much, much faster than using pragmas.
"
Class {
	#name : #AbstractSpotterProcessor,
	#superclass : #Object,
	#category : #'GT-Spotter-Model'
}

{ #category : #accessing }
AbstractSpotterProcessor class >> allEnabledSubclasses [
	
	^ self allSubclasses select: [ :e | e isEnabled ]
]

{ #category : #testing }
AbstractSpotterProcessor class >> isEnabled [ 

	^ self subclassResponsibility 
]
