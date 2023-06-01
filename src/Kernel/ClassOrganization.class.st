"
This class manages the class comment and a protocol organizer
"
Class {
	#name : #ClassOrganization,
	#superclass : #Object,
	#instVars : [
		'organizedClass'
	],
	#category : #'Kernel-Protocols'
}

{ #category : #'backward compatibility' }
ClassOrganization >> allMethodSelectors [

	self deprecated: 'Use #selectors instead' transformWith: '`@rcv allMethodSelectors' -> '`@rcv selectors'.
	^ self selectors
]

{ #category : #accessing }
ClassOrganization >> organizedClass [

	^ organizedClass ifNil: [ self error: 'ClassOrganization should always have an organized class associated.' ]
]

{ #category : #inlined }
ClassOrganization >> removeElement: aSelector [
	self deprecated: 'Use #removeFromProtocols: instead.' transformWith: '`@rcv removeElement: `@arg' -> '`@rcv removeFromProtocols: `@arg'. 
	^ self removeFromProtocols: aSelector
]

{ #category : #initialization }
ClassOrganization >> reset [

	self organizedClass resetProtocols
]

{ #category : #'backward compatibility' }
ClassOrganization >> setSubject: anObject [

	organizedClass := anObject
]
