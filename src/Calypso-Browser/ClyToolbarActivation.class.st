"
I annotate commands which should be placed in the toolbar.

By default I use #defaultMenuItemName to be used as a title of toolbar UI elements.
But if you want the title which is different from context menu then you should override #defaultToolbarItemName in the instance side of command
"
Class {
	#name : #ClyToolbarActivation,
	#superclass : #CmdMenuCommandActivationStrategy,
	#category : #'Calypso-Browser-Toolbar'
}

{ #category : #accessing }
ClyToolbarActivation >> menuItemNameFor: aCommand [
	^ menuItemName ifNil: [ aCommand defaultToolbarItemName ]
]
