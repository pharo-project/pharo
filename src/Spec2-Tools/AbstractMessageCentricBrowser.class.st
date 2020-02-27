"
I am an abstrct class for browsers centered around messages such as MessageBrowser or VersionBrowser.
"
Class {
	#name : #AbstractMessageCentricBrowser,
	#superclass : #SpPresenter,
	#instVars : [
		'messageList',
		'textConverter',
		'toolbarPresenter'
	],
	#category : #'Spec2-Tools-VersionBrowser'
}

{ #category : #initialization }
AbstractMessageCentricBrowser >> initialize [
	textConverter := SourceMethodConverter new.
	super initialize
]

{ #category : #initialization }
AbstractMessageCentricBrowser >> initializePresenters [
	messageList := self instantiate: MessageList.
	toolbarPresenter := self instantiate: SpMethodToolbar
]

{ #category : #initialization }
AbstractMessageCentricBrowser >> initializeWindow: aWindowPresenter [
	aWindowPresenter initialExtent: (900 min: self currentWorld extent x) @ (550 min: self currentWorld extent y)
]

{ #category : #private }
AbstractMessageCentricBrowser >> isMethodDefinition: anItem [
	"an item can be nil, a compiled method, or a ring object. Whereas a ring object can be a ring method or comment definition.
	 Some operations are only valid for compiled methods or ring object that are a method definitions"

	anItem ifNil: [ ^ false ].
	
	"assume it is a compiled method or ring method definition"
	^ (anItem isRingObject and: [ anItem isMethod and: [ anItem compiledMethod isNotNil ] ])
		or: [  anItem isCompiledMethod ]
]

{ #category : #api }
AbstractMessageCentricBrowser >> selectedMessage [
	^ messageList selectedMessage
]

{ #category : #accessing }
AbstractMessageCentricBrowser >> textConverter [
	^ textConverter
]

{ #category : #accessing }
AbstractMessageCentricBrowser >> textConverter: aTextConverter [
	textConverter := aTextConverter method: self textConverter method
]
