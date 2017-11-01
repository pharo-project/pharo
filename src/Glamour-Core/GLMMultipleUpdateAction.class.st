"
This class simply specifies that the updating announcement could come from any of the objects in the announcer objects collection.
"
Class {
	#name : #GLMMultipleUpdateAction,
	#superclass : #GLMUpdateAction,
	#category : 'Glamour-Core'
}

{ #category : #public }
GLMMultipleUpdateAction >> computeAnnouncerObjects [ 
	^ self transformation glamourValue: self presentation entity
]
