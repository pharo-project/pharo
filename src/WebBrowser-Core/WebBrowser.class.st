"
General access to a webbrowser
"
Class {
	#name : #WebBrowser,
	#superclass : #Object,
	#category : #'WebBrowser-Core-Base'
}

{ #category : #examples }
WebBrowser class >> example [

	self openOn: 'http://www.pharo.org'
]

{ #category : #testing }
WebBrowser class >> isForCurrentSystem [

	^self subclassResponsibility 
]

{ #category : #private }
WebBrowser class >> openNativeWebBrowserOn: aURL [

	self subclassResponsibility
]

{ #category : #'instance creation' }
WebBrowser class >> openOn: aURLString [
	"Open the webbrowser on the given URL"
	
	^self webbrowserClassForPlatform 
				openNativeWebBrowserOn: aURLString	

]

{ #category : #'private - accessing' }
WebBrowser class >> webbrowserClassForPlatform [

  ^ self allSubclasses 
				detect: #isForCurrentSystem 
				ifNone: [ self error: 'No web browser subclass for platform provided' ]
]
