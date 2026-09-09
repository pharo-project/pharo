"
I am the base class for DebugPoint Announcements that are required to update UIs when a debug point has been added/changed/removed.
"
Class {
	#name : 'DebugPointAnnouncement',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'debugPoint',
		'nodes',
		'methods'
	],
	#category : 'DebugPoints-Announcements',
	#package : 'DebugPoints',
	#tag : 'Announcements'
}

{ #category : 'instance creation' }
DebugPointAnnouncement class >> on: aDebugPoint nodes: nodes [
	^ self new
		debugPoint: aDebugPoint;
		nodes: nodes;
		yourself
]

{ #category : 'accessing' }
DebugPointAnnouncement >> debugPoint [
	^debugPoint 
]

{ #category : 'accessing' }
DebugPointAnnouncement >> debugPoint: aDebugPoint [
	debugPoint := aDebugPoint 
]

{ #category : 'accessing' }
DebugPointAnnouncement >> methods [

	^ methods ifNil: [
		  methods := nodes flatCollect: [ :entity | entity usingMethods ] ]
]

{ #category : 'accessing' }
DebugPointAnnouncement >> nodes [
	^nodes
]

{ #category : 'accessing' }
DebugPointAnnouncement >> nodes: someNodes [
	nodes := someNodes 
]
