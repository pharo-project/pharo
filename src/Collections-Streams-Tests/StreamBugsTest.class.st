"
SUnit test for stream bugs
"
Class {
	#name : 'StreamBugsTest',
	#superclass : 'TestCase',
	#category : 'Collections-Streams-Tests-Base',
	#package : 'Collections-Streams-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
StreamBugsTest >> testReadWriteStreamNextNBug [
	| aStream |
	aStream := ReadWriteStream on: String new.
	aStream nextPutAll: 'Hello World'.
	aStream next: 5
]
