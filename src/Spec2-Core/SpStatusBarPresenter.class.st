"
A presenter to render a StatusBar.
A status bar is a simple component that will push messages to be shown in the bar.
In general, this is used to present information to the user in a non-intrusive way, hence it is usually less important (otherwise you should use dialogs, etc.)

A status bar acts as a stack of messages that will be shown to the user. 
The messages can be pushed (See `SpStatusBarPresenter>>#pushMessage:`) and then popped (See `SpStatusBarPresenter>>#popMessage`). 
"
Class {
	#name : #SpStatusBarPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'message'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpStatusBarPresenter class >> adapterName [

	^ #StatusBarAdapter
]

{ #category : #documentation }
SpStatusBarPresenter class >> documentFactoryMethodSelector [

	^ #newStatusBar
]

{ #category : #testing }
SpStatusBarPresenter >> canTakeKeyboardFocus [
	"Answer whether the presenter can be focused or not. 
	 In case of `SpStatusBarPresenter`, this message will answer always false."

	^ false
]

{ #category : #api }
SpStatusBarPresenter >> message [
	"Answer current shown message."

	^ message
]

{ #category : #private }
SpStatusBarPresenter >> message: aString [
	message := aString
]

{ #category : #api }
SpStatusBarPresenter >> popMessage [ 
	"Pop current message (it will dismiss current message and restore previous one)"
	
	self message: nil.
	self changed: #popMessage
]

{ #category : #api }
SpStatusBarPresenter >> pushMessage: aString [ 
	"Push a new message to the stack (replacing current message with a new one)."
	
	self message: aString.
	self changed: #pushMessage
]
