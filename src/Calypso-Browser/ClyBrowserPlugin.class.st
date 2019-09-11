"
I am a root of the hierarchy of browser plugins.

My subclasses have two responsibilities:

- they can decorate browser tabs
- they can restrict table decorations

To decorate browser tab my subclasses should implement

- decorateTool: aBrowserTool 

Argument is normal morph. So subclasses can do with it whatever they want: 
- add new widgets 
- style some properties 
- force some selections

Plugins can be used to restrict table decoration of the browser.
Concrete plugin package can provide special kinds of table decorators. And it can be improtant to activate them only when particular browser plugin is actually installed into the browser.
So plugin itself do not need to implement anything special for this. Other objects can check that browser includes it.

By default new browser instances collect all auto activated plugins. It is based on method class side #isAutoActived which is true by default.
So as soon as you create new plugin class it will be present in new browser instances.
You can enable/disable plugin manually:

	ClyBrowserPlugin disable. 
	ClyBrowserPlugin enable.
	
I have class side variable #isAutoActivated which keeps this state.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
"
Class {
	#name : #ClyBrowserPlugin,
	#superclass : #Object,
	#instVars : [
		'browser'
	],
	#classInstVars : [
		'isAutoActivated'
	],
	#category : #'Calypso-Browser-UI'
}

{ #category : #testing }
ClyBrowserPlugin class >> disable [
	isAutoActivated := false
]

{ #category : #testing }
ClyBrowserPlugin class >> enable [
	isAutoActivated := true
]

{ #category : #testing }
ClyBrowserPlugin class >> isAutoActivated [
	^isAutoActivated ifNil: [ true ]
]

{ #category : #accessing }
ClyBrowserPlugin >> browser [
	^ browser
]

{ #category : #accessing }
ClyBrowserPlugin >> browser: anObject [
	browser := anObject
]

{ #category : #'tools decoration' }
ClyBrowserPlugin >> decorateTool: aBrowserTool [
]

{ #category : #testing }
ClyBrowserPlugin >> isAutoActivated [
	^self class isAutoActivated
]

{ #category : #testing }
ClyBrowserPlugin >> isSlow [
	^false
]

{ #category : #accessing }
ClyBrowserPlugin >> priority [
	^100
]
