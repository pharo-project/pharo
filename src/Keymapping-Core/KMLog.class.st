"
I am a simple log utility that can be enabled by sending the #setDebug and disabled by using #removeDebug. I trace the keymapping events.
"
Class {
	#name : #KMLog,
	#superclass : #Object,
	#classInstVars : [
		'debug'
	],
	#category : #'Keymapping-Core-Debugging'
}

{ #category : #'class initialization' }
KMLog class >> initialize [ 
	debug := false
]

{ #category : #logging }
KMLog class >> log: aKeyEvent [
	debug 	ifFalse: [ ^ self ].
	self traceCr: aKeyEvent printString
]

{ #category : #logging }
KMLog class >> logCompleteMatch: aKeymap [
	debug
		ifFalse: [ ^ self ].
	self traceCr: 'Complete match: ' , aKeymap printString
]

{ #category : #logging }
KMLog class >> logCompleteMatchBetween: aMorph and: aKeymap [
	debug 
		ifFalse: [ ^ self ].
	self
		traceCr: aMorph printString , ' Complete match: ' , aKeymap printString
]

{ #category : #logging }
KMLog class >> logPartialMatch: anEvent [
	debug 
		ifFalse: [ ^ self ].
	self traceCr: 'Partial match: ' , anEvent printString
]

{ #category : #debugging }
KMLog class >> removeDebug [
	<script>
	debug := false
]

{ #category : #debugging }
KMLog class >> setDebug [
	<script>
	debug := true
]
