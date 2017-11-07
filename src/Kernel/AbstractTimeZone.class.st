"
I am an abstract superclass for timezones.
See my subclasses for specific implementations.

Timezones are used to encapsulate the offset from the Coordinated Univeral Time (UTC) used for proper Date and Time display and manipulations.
"
Class {
	#name : #AbstractTimeZone,
	#superclass : #Object,
	#pools : [
		'ChronologyConstants'
	],
	#category : #Kernel-Chronology
}

{ #category : #accessing }
AbstractTimeZone >> abbreviation [
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractTimeZone >> name [
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractTimeZone >> offset [
	"Return a duration representing the offset from UTC for this timezone"
	self subclassResponsibility
]

{ #category : #printing }
AbstractTimeZone >> printOn: aStream [
 
 	super printOn: aStream.
 	aStream
 		nextPut: $(;
 		nextPutAll: self abbreviation;
 		nextPut: $).
]
