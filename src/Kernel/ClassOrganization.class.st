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

{ #category : #'instance creation' }
ClassOrganization class >> forClass: aClass [
	^ self basicNew
		initializeClass: aClass;
		yourself
]

{ #category : #'backward compatibility' }
ClassOrganization >> allMethodSelectors [

	self deprecated: 'Use #selectors on the organized class' transformWith: '`@rcv allMethodSelectors' -> '`@rcv organizedClass selectors'.
	^ self organizedClass selectors
]

{ #category : #copying }
ClassOrganization >> copyFrom: otherOrganization [

	self organizedClass protocols:  otherOrganization protocols copy
]

{ #category : #initialization }
ClassOrganization >> initializeClass: aClass [

	self initialize.
	organizedClass := aClass.
	organizedClass selectors do: [ :each | self organizedClass classify: each under: Protocol unclassified ]
]

{ #category : #accessing }
ClassOrganization >> organizedClass [

	^ organizedClass ifNil: [ self error: 'ClassOrganization should always have an organized class associated.' ]
]

{ #category : #accessing }
ClassOrganization >> protocols [

	^ self organizedClass protocols
]

{ #category : #inlined }
ClassOrganization >> removeElement: aSelector [
	self deprecated: 'Use #removeFromProtocols: from the organized class instead.' transformWith: '`@rcv removeElement: `@arg' -> '`@rcv organizedClass removeFromProtocols: `@arg'. 
	^ self organizedClass removeFromProtocols: aSelector
]

{ #category : #initialization }
ClassOrganization >> reset [

	self organizedClass resetProtocols
]

{ #category : #'backward compatibility' }
ClassOrganization >> setSubject: anObject [

	organizedClass := anObject
]
