"
I am an announcement that is sent when a DebugPoint targeting a variable has been hit.
I hold the value of the variable that has been read or hit
"
Class {
	#name : 'VariableDebugPointHit',
	#superclass : 'DebugPointHit',
	#instVars : [
		'variableValue'
	],
	#category : 'DebugPoints-Announcements',
	#package : 'DebugPoints',
	#tag : 'Announcements'
}

{ #category : 'accessing' }
VariableDebugPointHit >> variableValue [

	^ variableValue
]

{ #category : 'accessing' }
VariableDebugPointHit >> variableValue: anObject [

	variableValue := anObject
]
