"
I group commands in the browser toolbar which should be represented as general button.
I allow to place all button based commands at the end of toolbar.
"
Class {
	#name : #ClyButtonToolbarGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Calypso-Browser-Toolbar'
}

{ #category : #accessing }
ClyButtonToolbarGroup >> order [
	^5000
]
