"
Access to the web browser on Unix
"
Class {
	#name : #WBUnixWebBrowser,
	#superclass : #WebBrowser,
	#category : #'WebBrowser-Core-OS-Unix'
}

{ #category : #examples }
WBUnixWebBrowser class >> example [

	self openOn: 'http://www.pharo.org'
]

{ #category : #testing }
WBUnixWebBrowser class >> isForCurrentSystem [

	^Smalltalk os isUnix  
]

{ #category : #'instance creation' }
WBUnixWebBrowser class >> openOn: anURLString [
	"
	self openOn: 'http://www.pharo.org'
	"
	| openCommand |
	openCommand := 'xdg-open {1} &' format: { anURLString }.
	self system: openCommand.
]

{ #category : #'private - uffi' }
WBUnixWebBrowser class >> system: command [

	^LibC system: command 
]
