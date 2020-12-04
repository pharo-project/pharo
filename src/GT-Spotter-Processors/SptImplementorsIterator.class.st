"
I implement an iterator over all the implementors in the system
"
Class {
	#name : #SptImplementorsIterator,
	#superclass : #SptGenericGenerator,
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #enumerating }
SptImplementorsIterator >> elementsDo: aValuable [

	SystemNavigation default
		allBehaviorsDo: [ :aClass | aClass methodsDo: aValuable ]
]
