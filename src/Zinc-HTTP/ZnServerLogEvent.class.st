"
I am ZnServerLogEvent, a ZnLogEvent generated by ZnServer and its subclasses.

I hold a processId attribute.
I hold an optional serverId attribute.
"
Class {
	#name : 'ZnServerLogEvent',
	#superclass : 'ZnLogEvent',
	#instVars : [
		'serverId'
	],
	#category : 'Zinc-HTTP-Logging',
	#package : 'Zinc-HTTP',
	#tag : 'Logging'
}

{ #category : 'initialization' }
ZnServerLogEvent >> initialize [
	super initialize.
	processId := ZnUtils currentProcessID
]

{ #category : 'printing' }
ZnServerLogEvent >> printHeaderOn: stream [
	super printHeaderOn: stream.
	serverId ifNotNil: [ stream space; << serverId ]
]

{ #category : 'accessing' }
ZnServerLogEvent >> serverId [
	^ serverId
]

{ #category : 'initialize' }
ZnServerLogEvent >> serverId: anObject [
	serverId := anObject
]
