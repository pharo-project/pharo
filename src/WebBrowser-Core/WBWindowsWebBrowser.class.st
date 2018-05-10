"
Provides native access to the web browser on Windows. Do not use directly, use class WebBrowser instead.
"
Class {
	#name : #WBWindowsWebBrowser,
	#superclass : #WebBrowser,
	#category : #'WebBrowser-Core-OS-Windows'
}

{ #category : #testing }
WBWindowsWebBrowser class >> isForCurrentSystem [

	^Smalltalk os isWindows   
]

{ #category : #private }
WBWindowsWebBrowser class >> openNativeWebBrowserOn: anURLString [

	^self shellExecute: 'open' file: anURLString parameters: '' directory: '' show: 5 "SW_SHOW"
]

{ #category : #'private - uffi' }
WBWindowsWebBrowser class >> shellExecute: lpOperation file: lpFile parameters: lpParameters directory: lpDirectory show: nShowCmd [	
	
	^self ffiCall: #( 
			FFIConstantHandle ShellExecuteA(
     				0,
     				String* lpOperation,
         			String* lpFile,
     				String* lpParameters,
     				String* lpDirectory,
        			int nShowCmd)) module: #shell32
]
