"
This announcement will be emitted when a method is removed from a trait or a class, 
using ClassDescription >>removeSelector: or TraitDescription >> removeSelector.

"
Class {
	#name : 'MethodRemoved',
	#superclass : 'MethodAnnouncement',
	#instVars : [
		'protocol',
		'methodOrigin',
		'methodPackage'
	],
	#category : 'System-Announcements-System-Methods',
	#package : 'System-Announcements',
	#tag : 'System-Methods'
}

{ #category : 'instance creation' }
MethodRemoved class >> methodRemoved: aCompiledMethod protocol: aProtocol origin: aBehavior package: aPackage [

	^ self new
		  method: aCompiledMethod;
		  protocol: aProtocol;
		  methodOrigin: aBehavior;
		  methodPackage: aPackage;
		  yourself
]

{ #category : 'testing' }
MethodRemoved >> affectsMethodsInProtocol: aProtocol [

	^ self protocol name = aProtocol
]

{ #category : 'accessing' }
MethodRemoved >> methodOrigin [
	^ methodOrigin
]

{ #category : 'accessing' }
MethodRemoved >> methodOrigin: anObject [
	methodOrigin := anObject
]

{ #category : 'accessing' }
MethodRemoved >> methodPackage [

	^ methodPackage
]

{ #category : 'accessing' }
MethodRemoved >> methodPackage: aPackage [

	methodPackage := aPackage
]

{ #category : 'accessing' }
MethodRemoved >> methodRemoved [

	^ method
]

{ #category : 'accessing' }
MethodRemoved >> protocol [
	^ protocol
]

{ #category : 'accessing' }
MethodRemoved >> protocol: anObject [

	protocol := anObject
]
