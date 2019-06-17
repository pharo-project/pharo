"
I am a root of commands which are supposed to switch browser mode.
My instances are represented in browser toolbar as radio buttons which state reflects the ""applied status"" of the command.
So subclasses should implement the method #isAppliedToBrowser.

The radio button is wraped by ClyBrowserModeSwitch widget.

Also I force browser toolbar update after execution
"
Class {
	#name : #ClySwitchBrowserModeCommand,
	#superclass : #ClyBrowserCommand,
	#category : #'Calypso-Browser-Commands'
}

{ #category : #testing }
ClySwitchBrowserModeCommand class >> isAbstract [
	^self = ClySwitchBrowserModeCommand 
]

{ #category : #execution }
ClySwitchBrowserModeCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	browser rebuildToolbar
]

{ #category : #'activation-toolbar support' }
ClySwitchBrowserModeCommand >> createToolbarItemFor: aCommandActivator [

	^ClyBrowserModeSwitchMorph activatingBy: aCommandActivator
]

{ #category : #testing }
ClySwitchBrowserModeCommand >> isAppliedToBrowser [
	self subclassResponsibility 
]
