"
I am menu group to arrange together breakpoint commands
"
Class {
	#name : 'ClyDebuggingMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Commands',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClyDebuggingMenuGroup >> isInlined [
	^true
]

{ #category : 'accessing' }
ClyDebuggingMenuGroup >> order [
	^1.5
]
