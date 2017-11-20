Class {
	#name : #GLMTestPane,
	#superclass : #GLMPane,
	#instVars : [
		'seenTransmissions'
	],
	#category : #Glamour-Tests-Core
}

{ #category : #private }
GLMTestPane class >> new [

	^super new initialize
]

{ #category : #private }
GLMTestPane class >> portClass [ 
	 
	^GLMTestPort
]

{ #category : #accessing }
GLMTestPane >> browser [
	browser isNil ifTrue: [ ^ GLMNoBrowser new ].
	^ browser
]

{ #category : #accessing }
GLMTestPane >> browser: aBrowser [ 
	browser := aBrowser
]
