Class {
	#name : #MockCriticBrowserFalsePositiveButton,
	#superclass : #CBCriticBrowser,
	#instVars : [
		'messageCall'
	],
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #initialize }
MockCriticBrowserFalsePositiveButton >> initialize [ 
	super initialize.
	messageCall := 0.
]

{ #category : #private }
MockCriticBrowserFalsePositiveButton >> markAsFalsePositiveForCurrentRule: anItem [
	messageCall := messageCall + 1
]

{ #category : #accessing }
MockCriticBrowserFalsePositiveButton >> messageCall [
	^ messageCall
]
