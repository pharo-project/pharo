"
I decide whether a deprecation that has a rewriting rule rewrites the caller of the deprecated method. My subclasses are the choices of the Deprecation handling settings: never, at the end of the compatibility period of PHEP-0004, or always.
"
Class {
	#name : 'DeprecationRewriting',
	#superclass : 'Object',
	#category : 'Deprecation-Strategies',
	#package : 'Deprecation',
	#tag : 'Strategies'
}

{ #category : 'accessing' }
DeprecationRewriting class >> label [
	"The name of this choice in the settings."

	^ self subclassResponsibility
]

{ #category : 'printing' }
DeprecationRewriting class >> printOn: aStream [

	self == DeprecationRewriting ifTrue: [ ^ super printOn: aStream ].
	aStream nextPutAll: self label
]

{ #category : 'testing' }
DeprecationRewriting class >> shouldRewrite: anAutomaticRewriting [
	"Whether anAutomaticRewriting, a deprecation that has a rewriting rule, rewrites the caller of the deprecated method."

	^ self subclassResponsibility
]
