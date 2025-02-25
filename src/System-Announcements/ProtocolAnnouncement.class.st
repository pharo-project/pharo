"
I'm an abstract announcement for protocols
"
Class {
	#name : 'ProtocolAnnouncement',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'protocol',
		'classReorganized'
	],
	#category : 'System-Announcements-System-Protocols',
	#package : 'System-Announcements',
	#tag : 'System-Protocols'
}

{ #category : 'instance creation' }
ProtocolAnnouncement class >> class: aClass [

	^ self new
		  classReorganized: aClass;
		  yourself
]

{ #category : 'instance creation' }
ProtocolAnnouncement class >> in: aClass protocol: aProtocol [

	^ (self class: aClass)
		  protocol: aProtocol;
		  yourself
]

{ #category : 'accessing' }
ProtocolAnnouncement >> classAffected [

	^ self classReorganized
]

{ #category : 'accessing' }
ProtocolAnnouncement >> classReorganized [
	^ classReorganized
]

{ #category : 'accessing' }
ProtocolAnnouncement >> classReorganized: anObject [
	classReorganized := anObject
]

{ #category : 'accessing' }
ProtocolAnnouncement >> protocol [
	^ protocol
]

{ #category : 'accessing' }
ProtocolAnnouncement >> protocol: aString [
	protocol := aString
]
