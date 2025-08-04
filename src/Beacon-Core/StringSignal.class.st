"
This is a simple signal that holds a message.

###Usage:
```
	(StringSignal emit: 'This is a message') message
```	

	

"
Class {
	#name : 'StringSignal',
	#superclass : 'BeaconSignal',
	#instVars : [
		'message'
	],
	#category : 'Beacon-Core-Signals',
	#package : 'Beacon-Core',
	#tag : 'Signals'
}

{ #category : 'instance creation' }
StringSignal class >> emit: aString [
	^ self new
		message: aString;
		emit
]

{ #category : 'accessing' }
StringSignal >> message [

	^ message
]

{ #category : 'accessing' }
StringSignal >> message: anObject [

	message := anObject
]

{ #category : 'printing' }
StringSignal >> printOneLineContentsOn: stream [

	stream nextPutAll: self message
]

{ #category : 'accessing' }
StringSignal >> target [
	"make sure that String signal is polymorph with WrapperSignal"
	^ message 
]
