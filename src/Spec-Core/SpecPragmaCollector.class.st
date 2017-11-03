"
A SpecPragmaCollector is a specialized PragmaCollector.

Sonce we just want specs defined on class side, we do not have to check the whole system, then to restrain.
"
Class {
	#name : #SpecPragmaCollector,
	#superclass : #PragmaCollector,
	#instVars : [
		'behavior'
	],
	#classInstVars : [
		'behavior'
	],
	#category : #Spec-Core-Support
}

{ #category : #'instance creation' }
SpecPragmaCollector class >> behavior: aBehavior [

	^ self new
		behavior: aBehavior;
		yourself
]

{ #category : #private }
SpecPragmaCollector >> allPragmas [

	^ self allPragmasIn: (self behavior ifNil: [ ^ #() ])
]

{ #category : #private }
SpecPragmaCollector >> allPragmasIn: aClass [

	^ (Array
		streamContents: [:stream | Pragma
									withPragmasIn: aClass
									do: [:pragma | stream nextPut: pragma ]]) 
]

{ #category : #accessing }
SpecPragmaCollector >> behavior [
	^ behavior
]

{ #category : #accessing }
SpecPragmaCollector >> behavior: anObject [
	behavior := anObject
]

{ #category : #'system changes' }
SpecPragmaCollector >> installSystemNotifications [
	"We do not need to register anything for spec, and if we do it will make it really slow"
]

{ #category : #initializing }
SpecPragmaCollector >> reset [
	"reinitialize current system settings"
	self
		noMoreAnnounceWhile: [self collected copy
				do: [:pragma | self removePragma: pragma].
			self allPragmas
				do: [:pragma | self addPragma: pragma]].
	self announce: (PragmaCollectorReset collector: self)
]
