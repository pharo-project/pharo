"
An AllProtocol is a special protocol to handle the ""all"" case
"
Class {
	#name : #AllProtocol,
	#superclass : #AbstractProtocol,
	#instVars : [
		'classOrganization'
	],
	#category : #'Kernel-Protocols'
}

{ #category : #'instance creation' }
AllProtocol class >> classOrganization: classOrganization [

	^ self new
		classOrganization: classOrganization;
		yourself
]

{ #category : #data }
AllProtocol class >> defaultName [

	^ #'-- all --'
]

{ #category : #private }
AllProtocol >> canBeRemoved [
	^ false
]

{ #category : #testing }
AllProtocol >> canBeRenamed [
	^ false
]

{ #category : #accessing }
AllProtocol >> classOrganization [

	^ classOrganization
]

{ #category : #accessing }
AllProtocol >> classOrganization: anObject [

	classOrganization := anObject
]

{ #category : #testing }
AllProtocol >> isVirtualProtocol [
	^ true
]

{ #category : #accessing }
AllProtocol >> methodSelectors [

	^ self classOrganization allMethodSelectors
]

{ #category : #accessing }
AllProtocol >> name [

	^ (self isEmpty and: [ self classOrganization protocols isEmpty ])
		  ifTrue: [ self class nullProtocolName ]
		  ifFalse: [ self class defaultName ]
]

{ #category : #accessing }
AllProtocol >> name: aString [
	self error: 'You cannot change my name'
]
