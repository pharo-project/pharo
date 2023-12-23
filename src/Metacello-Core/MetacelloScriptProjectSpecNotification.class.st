Class {
	#name : 'MetacelloScriptProjectSpecNotification',
	#superclass : 'MetacelloScriptNotification',
	#instVars : [
		'projectSpec'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'exception description' }
MetacelloScriptProjectSpecNotification >> defaultAction [
    "Result of signal should be the projectSpec to be used to perform the load"

    ^ self projectSpec
]

{ #category : 'accessing' }
MetacelloScriptProjectSpecNotification >> projectSpec [
    ^ projectSpec
]

{ #category : 'accessing' }
MetacelloScriptProjectSpecNotification >> projectSpec: anObject [
    projectSpec := anObject
]
