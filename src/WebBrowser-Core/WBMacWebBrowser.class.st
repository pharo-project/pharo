"
Provides native access to the web browser on Mac. Do not use directly, use class WebBrowser instead.
"
Class {
	#name : #WBMacWebBrowser,
	#superclass : #WebBrowser,
	#category : #'WebBrowser-Core-OS-Mac'
}

{ #category : #testing }
WBMacWebBrowser class >> isForCurrentSystem [

	^Smalltalk os isMacOS 
]

{ #category : #private }
WBMacWebBrowser class >> openNativeWebBrowserOn: anURLString [

	| openCommand |
	openCommand := 'open "{1}"' format: { anURLString }.
	self system: openCommand
]

{ #category : #'private - uffi' }
WBMacWebBrowser class >> system: command [

	^LibC system: command 
]
