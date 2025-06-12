"
I am a class used to open a link in the default web browser installed on the OS.

I'll delegate the opening to the current OS platform through the platform visitor.
"
Class {
	#name : 'WebBrowser',
	#superclass : 'OSPlatformVisitor',
	#instVars : [
		'url'
	],
	#category : 'WebBrowser-Core-Base',
	#package : 'WebBrowser-Core',
	#tag : 'Base'
}

{ #category : 'examples' }
WebBrowser class >> example [

	self openOn: 'http://pharo.org'
]

{ #category : 'instance creation' }
WebBrowser class >> openOn: anURLOrString [
	"Open the webbrowser on the given URL or String"

	"^ Smalltalk os openNativeWebBrowserOn: anURLOrString asString"

	^ self new
		  url: anURLOrString asString;
		  visit
]

{ #category : 'windows' }
WebBrowser >> shellExecute: lpOperation file: lpFile parameters: lpParameters directory: lpDirectory show: nShowCmd [

	^ self ffiCall: #(
			FFIConstantHandle ShellExecuteA(
     				int 0, "Operation is not associated with a window"
     				String lpOperation,
         			String lpFile,
     				String lpParameters,
     				String lpDirectory,
        			int nShowCmd)) module: #shell32
]

{ #category : 'accessing' }
WebBrowser >> url: anObject [
	url := anObject
]

{ #category : 'visiting' }
WebBrowser >> visitMacOS: aPlatform [

	| openCommand |
	openCommand := 'open "{1}"' format: { url }.
	LibC system: openCommand
]

{ #category : 'visiting' }
WebBrowser >> visitUnix: aPlatform [

	| openCommand |
	openCommand := 'xdg-open ''{1}'' &' format: { url }.
	LibC system: openCommand
]

{ #category : 'visiting' }
WebBrowser >> visitWindows: aPlatform [

	^ self
		  shellExecute: 'open'
		  file: url
		  parameters: ''
		  directory: ''
		  show: 5 "SW_SHOW"
]
