"
I am menu group to arrange together breakpoint commands
"
Class {
	#name : #ClyDebuggingMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Commands'
}

{ #category : #testing }
ClyDebuggingMenuGroup >> isInlined [
	^true
]

{ #category : #accessing }
ClyDebuggingMenuGroup >> order [
	^1.5
]
