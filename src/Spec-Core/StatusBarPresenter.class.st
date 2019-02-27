"
I'm a presenter to render a statusBar.
A status bar is a simple component that will push messages to be shown in the bar.
In general, this is used to present information to the user in a non-intrusive way, hence it is usually less important (otherwise you should use dialogs, etc.)
"
Class {
	#name : #StatusBarPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'messageHolder'
	],
	#category : #'Spec-Core-Widgets-Toolbar'
}

{ #category : #specs }
StatusBarPresenter class >> adapterName [

	^ #StatusBarAdapter
]

{ #category : #initialization }
StatusBarPresenter >> initialize [

	super initialize.
	messageHolder := nil asValueHolder
]

{ #category : #accessing }
StatusBarPresenter >> message [

	^ messageHolder value
]

{ #category : #private }
StatusBarPresenter >> message: aString [

	messageHolder value: aString
]

{ #category : #accessing }
StatusBarPresenter >> popMessage [ 
	
	self message: nil.
	self changed: #popMessage
]

{ #category : #accessing }
StatusBarPresenter >> pushMessage: aString [ 
	
	self message: aString.
	self changed: #pushMessage
]
