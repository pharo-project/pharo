"
A factory that produces instances of RG2Behavior composed with RGMetaclassStrategy
"
Class {
	#name : #RGMetaclass,
	#superclass : #RGBehaviorFactory,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'instance creation' }
RGMetaclass class >> newStrategy [

	^ RGMetaclassStrategy unresolved
]
