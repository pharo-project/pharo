"
I am STONZnUrl, I am an implementation artifact to help reading objects with class tag equal to my #stonName.

I am a ZnUrl.
"
Class {
	#name : 'STONZnUrl',
	#superclass : 'ZnUrl',
	#category : 'STON-Core-Reader',
	#package : 'STON-Core',
	#tag : 'Reader'
}

{ #category : 'instance creation' }
STONZnUrl class >> fromSton: stonReader [
	^ ZnUrl fromString: stonReader parseListSingleton
]

{ #category : 'accessing' }
STONZnUrl class >> stonName [
	^ #URL
]
