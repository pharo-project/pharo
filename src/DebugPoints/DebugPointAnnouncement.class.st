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

{ #category : 'testing' }
DebugPointAnnouncement >> affectsMethod: aMethod [

	^ self methods anySatisfy: [:each |  each compiledMethod == aMethod compiledMethod.  ]
]

{ #category : 'testing' }
DebugPointAnnouncement >> affectsMethods [
	^true
]

{ #category : 'testing' }
DebugPointAnnouncement >> affectsMethodsDefinedInClass: aClass [
	^ self methods
		anySatisfy: [ :each | each methodClass isNotNil and: [ each origin == aClass ] ]
]

{ #category : 'testing' }
DebugPointAnnouncement >> affectsMethodsDefinedInPackage: aPackage [
	^ self methods
		anySatisfy:
			[ :each | each methodClass isNotNil and: [ each package == aPackage ] ]
]

{ #category : 'testing' }
DebugPointAnnouncement >> canAffectResultOfMethodQuery: aMethodQuery [

	^ self methods anySatisfy: [:each | 
		aMethodQuery isAffectedByChangedMethod: each ]
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
		  methods := nodes flatCollect: [ :entity |
			             (entity isKindOf: RBProgramNode)
				             ifTrue: [ { entity methodNode compiledMethod } ]
				             ifFalse: [ entity usingMethods ] ] ]
]

{ #category : 'accessing' }
DebugPointAnnouncement >> nodes [
	^nodes
]

{ #category : 'accessing' }
DebugPointAnnouncement >> nodes: someNodes [
	nodes := someNodes 
]
