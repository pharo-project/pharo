"
I decide how the system reacts when a deprecated method runs, besides recording it in the log that #deprecationsWhile: reads, which always happens. My subclasses are the choices of the Deprecation handling settings: ignore it, log it, warn, or signal an error.

The reaction is independent from rewriting the callers, which a DeprecationRewriting decides.
"
Class {
	#name : 'DeprecationReaction',
	#superclass : 'Object',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
DeprecationReaction class >> label [
	"The name of this reaction in the settings."

	^ self subclassResponsibility
]

{ #category : 'printing' }
DeprecationReaction class >> printOn: aStream [

	self == DeprecationReaction ifTrue: [ ^ super printOn: aStream ].
	aStream nextPutAll: self label
]

{ #category : 'reacting' }
DeprecationReaction class >> react: anAutomaticRewriting [
	"React to anAutomaticRewriting, a deprecation that was signaled and that no one handled."

	self subclassResponsibility
]
