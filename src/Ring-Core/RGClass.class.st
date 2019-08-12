"
A factory that produces instances of RG2Behavior composed with RG2ClassStrategy
"
Class {
	#name : #RGClass,
	#superclass : #RGBehaviorFactory,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'instance creation' }
RGClass class >> newStrategy [

	^ RGClassStrategy unresolved
]
