"
ZnServerErrorEvent is a ZnServerLogEvent related to an exception that occurred.
"
Class {
	#name : 'ZnServerErrorEvent',
	#superclass : 'ZnServerLogEvent',
	#instVars : [
		'exception'
	],
	#category : 'Zinc-HTTP-Logging',
	#package : 'Zinc-HTTP',
	#tag : 'Logging'
}

{ #category : 'accessing' }
ZnServerErrorEvent >> exception [
	^ exception
]

{ #category : 'accessing' }
ZnServerErrorEvent >> exception: anObject [
	exception := anObject
]
