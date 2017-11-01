"
I am ZnSignalProgress.
I am a DynamicVariable and a ProcessSpecificVariable.

I can be used to indicate that Zn related code down the call stack has to signal HTTPProgress, for example:

ZnSignalProgress 
	value: true 
	during: [ ^ ZnClient new get: 'http://zn.stfx.eu/zn/small.html' ]
"
Class {
	#name : #ZnSignalProgress,
	#superclass : #DynamicVariable,
	#category : 'Zinc-HTTP-Variables'
}

{ #category : #accessing }
ZnSignalProgress class >> enabled [
	"Answer if HTTPProgress should be signalled.
	The default is false."
	
	^ self value
		ifNil: [ false ]
		ifNotNil: [ :value | value ]
]
