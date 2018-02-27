"
Provides native access to the web browser on Unix. Do not use directly, use class WebBrowser instead.
"
Class {
	#name : #WBUnixWebBrowser,
	#superclass : #WebBrowser,
	#category : #'WebBrowser-Core-OS-Unix'
}

{ #category : #testing }
WBUnixWebBrowser class >> isForCurrentSystem [

	^Smalltalk os isUnix  
]

{ #category : #private }
WBUnixWebBrowser class >> openNativeWebBrowserOn: anURLString [

	| openCommand |
	openCommand := 'xdg-open {1} &' format: { anURLString }.
	self system: openCommand
]

{ #category : #'private - uffi' }
WBUnixWebBrowser class >> system: command [

	^LibC system: command 
]
