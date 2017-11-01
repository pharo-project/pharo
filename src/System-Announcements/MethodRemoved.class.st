"
This announcement will be emitted when a method is removed from a trait or a class, 
using ClassDescription >>removeSelector: or TraitDescription >> removeSelector.

"
Class {
	#name : #MethodRemoved,
	#superclass : #MethodAnnouncement,
	#instVars : [
		'protocol',
		'methodOrigin'
	],
	#category : 'System-Announcements-System-Methods'
}

{ #category : #'instance creation' }
MethodRemoved class >> methodRemoved: aCompiledMethod protocol: aProtocol origin: aBehavior [
	^self new
		method: aCompiledMethod;
		protocol: aProtocol;
		methodOrigin: aBehavior;
		yourself
]

{ #category : #accessing }
MethodRemoved >> methodOrigin [
	^ methodOrigin
]

{ #category : #accessing }
MethodRemoved >> methodOrigin: anObject [
	methodOrigin := anObject 
]

{ #category : #accessing }
MethodRemoved >> methodRemoved [
	
	^ method
]

{ #category : #accessing }
MethodRemoved >> protocol [
	^ protocol
]

{ #category : #accessing }
MethodRemoved >> protocol: anObject [
	
	protocol := anObject
]
