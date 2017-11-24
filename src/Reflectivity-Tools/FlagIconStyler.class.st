"
I'm in charge to style an ast when there is a message send of #flag:.
This is used to tag methods for TODOs, having many of these is a bad
sign...

	self flag: #Bob
"
Class {
	#name : #FlagIconStyler,
	#superclass : #IconStyler,
	#category : #'Reflectivity-Tools-Breakpoints'
}

{ #category : #defaults }
FlagIconStyler >> highlightColor [
	^(Color yellow alpha: 0.3)
]

{ #category : #defaults }
FlagIconStyler >> iconFor: aNode [
	^ self iconNamed: #flag.
]

{ #category : #defaults }
FlagIconStyler >> iconLabel: aNode [
	^''
]

{ #category : #testing }
FlagIconStyler >> shouldStyleNode: aNode [
	^aNode isMessage and: [aNode selector = #flag:]
]
