"
I am a simple log utility that can be enabled by sending the #setDebug and disabled by using #removeDebug. I trace the keymapping events.
"
Class {
	#name : #KMLog,
	#superclass : #Object,
	#classInstVars : [
		'debug'
	],
	#category : 'Keymapping-Core'
}

{ #category : #'as yet unclassified' }
KMLog class >> log: aKeyEvent [

	debug == true ifFalse: [ ^self ].
	
	self logger
		nextPutAll: aKeyEvent printString;
		cr;
		flush.
]

{ #category : #'as yet unclassified' }
KMLog class >> logCompleteMatch: aKeymap [

	debug == true ifFalse: [ ^self ].
	
	self logger 
		nextPutAll: 'Complete match: ';
		nextPutAll: aKeymap printString;
		cr;
		flush.
]

{ #category : #'as yet unclassified' }
KMLog class >> logCompleteMatchBetween: aMorph and: aKeymap [

	debug == true ifFalse: [ ^self ].
	
	self logger 
		nextPutAll: aMorph printString;
		nextPutAll: ' Complete match: ';
		nextPutAll: aKeymap printString;
		cr;
		flush.
]

{ #category : #'as yet unclassified' }
KMLog class >> logPartialMatch: anEvent [

	debug == true ifFalse: [ ^self ].
	
	self logger 
		nextPutAll: 'Partial match: ';
		nextPutAll: anEvent printString;
		cr;
		flush.
]

{ #category : #'as yet unclassified' }
KMLog class >> logger [

	^Transcript
]

{ #category : #'as yet unclassified' }
KMLog class >> removeDebug [
	debug := false
]

{ #category : #'as yet unclassified' }
KMLog class >> setDebug [
	debug := true
]
