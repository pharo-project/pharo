"
This announcement will be emitted when a method is removed from a trait or a class, 
using ClassDescription >>removeSelector:.

"
Class {
	#name : 'MethodRemoved',
	#superclass : 'MethodAnnouncement',
	#instVars : [
		'methodOrigin'
	],
	#category : 'System-Announcements-System-Methods',
	#package : 'System-Announcements',
	#tag : 'System-Methods'
}

{ #category : 'instance creation' }
MethodRemoved class >> methodRemoved: aCompiledMethod origin: aBehavior [

	^ self new
		  method: aCompiledMethod;
		  methodOrigin: aBehavior;
		  yourself
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
MethodRemoved >> methodRemoved [

	^ method
]
