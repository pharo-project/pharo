"
Access to the web browser on Mac
"
Class {
	#name : #WBMacWebBrowser,
	#superclass : #WebBrowser,
	#category : 'WebBrowser-Core-OS-Mac'
}

{ #category : #examples }
WBMacWebBrowser class >> example [

	self openOn: 'http://www.pharo.org'
]

{ #category : #testing }
WBMacWebBrowser class >> isForCurrentSystem [

	^Smalltalk os isMacOS 
]

{ #category : #'instance creation' }
WBMacWebBrowser class >> openOn: anURLString [
	"
	self openOn: 'http://www.pharo.org'
	"
	| openCommand |
	openCommand := 'open "{1}"' format: { anURLString }.
	self system: openCommand.
]

{ #category : #'private - uffi' }
WBMacWebBrowser class >> system: command [

	^LibC system: command 
]
