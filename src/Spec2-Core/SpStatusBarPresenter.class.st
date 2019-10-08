"
I'm a presenter to render a statusBar.
A status bar is a simple component that will push messages to be shown in the bar.
In general, this is used to present information to the user in a non-intrusive way, hence it is usually less important (otherwise you should use dialogs, etc.)
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

{ #category : #testing }
SpStatusBarPresenter >> canTakeKeyboardFocus [

	^ false
]

{ #category : #accessing }
SpStatusBarPresenter >> message [
	^ message
]

{ #category : #private }
SpStatusBarPresenter >> message: aString [
	message := aString
]

{ #category : #accessing }
SpStatusBarPresenter >> popMessage [ 
	
	self message: nil.
	self changed: #popMessage
]

{ #category : #accessing }
SpStatusBarPresenter >> pushMessage: aString [ 
	
	self message: aString.
	self changed: #pushMessage
]
