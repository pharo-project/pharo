"
I am a null display interface for testing the Ed debugger.
I catch all message through DNU and just do nothing.
"
Class {
	#name : #EDMockDisplayInterface,
	#superclass : #Object,
	#instVars : [
		'clear',
		'displayData'
	],
	#category : #'EmergencyDebugger-Tests'
}

{ #category : #'API - mock' }
EDMockDisplayInterface >> clear [
	clear := true
]

{ #category : #'API - mock' }
EDMockDisplayInterface >> displayData [
	^displayData ifNil:['']
]

{ #category : #'reflective operations' }
EDMockDisplayInterface >> doesNotUnderstand: aMessage [
	
	"self inform: 'Ed display: ', aMessage printString"
]

{ #category : #'API - mock' }
EDMockDisplayInterface >> isCleared [
	^clear ifNil:[false]
]

{ #category : #'API - mock' }
EDMockDisplayInterface >> show: aText [
	displayData := aText
]
