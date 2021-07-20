Class {
	#name : #MicArgumentsSplitter,
	#superclass : #Object,
	#instVars : [
		'arguments',
		'firstTag',
		'name',
		'stream'
	],
	#category : #'Microdown-Parser'
}

{ #category : #'instance creation' }
MicArgumentsSplitter class >> withStream: aStream [
	^ self new stream: aStream; yourself
]

{ #category : #'instance creation' }
MicArgumentsSplitter class >> withString: aString [
	^ self new stream: (aString readStream); yourself
]

{ #category : #accessing }
MicArgumentsSplitter >> arguments [
	^ arguments ifNil: [ ^ arguments := ZnResourceMetaUtils parseQueryFrom: stream ]
]

{ #category : #accessing }
MicArgumentsSplitter >> firstTag [ 
	^ firstTag ifNil: [ 
		firstTag := (stream upTo: $=) asSymbol. 
		stream position: 0.
		^ firstTag ]
]

{ #category : #accessing }
MicArgumentsSplitter >> name [
	^ name ifNil: [ ^ name := (stream upTo: self parserClass argumentListStartDelimiter first) contents asString; yourself ]
]

{ #category : #accessing }
MicArgumentsSplitter >> parserClass [
	^ MicroDownParser 
]

{ #category : #accessing }
MicArgumentsSplitter >> stream: aStream [
	stream := aStream
]
