"
I am a command to open browser on given class
"
Class {
	#name : #SycBrowseClassCommand,
	#superclass : #SycSingleClassCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #accessing }
SycBrowseClassCommand >> defaultMenuIconName [
	^#glamorousBrowse
]

{ #category : #accessing }
SycBrowseClassCommand >> defaultMenuItemName [
	^'Browse'
]

{ #category : #execution }
SycBrowseClassCommand >> execute [
	targetClass browse
]
