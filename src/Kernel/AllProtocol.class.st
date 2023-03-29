"
An AllProtocol is a special protocol to handle the ""all"" case
"
Class {
	#name : #AllProtocol,
	#superclass : #AbstractProtocol,
	#instVars : [
		'protocolOrganizer',
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

{ #category : #'instance creation' }
AllProtocol class >> protocolOrganizer: protocolOrganizer [

	^ self new
		protocolOrganizer: protocolOrganizer;
		yourself
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

	^ (self protocolOrganizer ifNil: [ self classOrganization ]) allMethodSelectors
]

{ #category : #accessing }
AllProtocol >> name [

	^ (self isEmpty and: [ (self protocolOrganizer ifNil: [ self classOrganization ]) protocols isEmpty ])
		  ifTrue: [ self class nullProtocolName ]
		  ifFalse: [ self class defaultName ]
]

{ #category : #accessing }
AllProtocol >> name: aString [
	self error: 'You cannot change my name'
]

{ #category : #accessing }
AllProtocol >> protocolOrganizer [

	^ protocolOrganizer
]

{ #category : #accessing }
AllProtocol >> protocolOrganizer: anObject [

	protocolOrganizer := anObject
]
