"
I am menu group to arrange together metalink and watchpoint commands
"
Class {
	#name : #ClyAdvancedDebuggingMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Commands'
}

{ #category : #testing }
ClyAdvancedDebuggingMenuGroup >> isInlined [
	^true
]

{ #category : #accessing }
ClyAdvancedDebuggingMenuGroup >> order [
	^1.8
]
