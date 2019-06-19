"
I am menu group to arrange together all metalink related commands (breakpoints, counters, watchpoints)
"
Class {
	#name : #ClyMetalinkMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Commands'
}

{ #category : #testing }
ClyMetalinkMenuGroup >> isInlined [
	^true
]

{ #category : #accessing }
ClyMetalinkMenuGroup >> order [
	^1.5
]
