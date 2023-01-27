"
An AllProtocol is a special protocol to handle the ""all"" case
"
Class {
	#name : #AllProtocol,
	#superclass : #AbstractProtocol,
	#instVars : [
		'protocolOrganizer'
	],
	#category : #'Kernel-Protocols'
}

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

{ #category : #testing }
AllProtocol >> isVirtualProtocol [
	^ true
]

{ #category : #accessing }
AllProtocol >> methodSelectors [
	^ self protocolOrganizer allMethodSelectors
]

{ #category : #accessing }
AllProtocol >> name [
	^ (self isEmpty and: [ protocolOrganizer protocols isEmpty ])
		ifTrue: [ self class nullCategory ]
		ifFalse: [  self class defaultName ]
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
