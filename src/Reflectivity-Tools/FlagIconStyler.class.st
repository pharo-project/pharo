"
I'm in charge to style an ast when there is a message send of #flag:.
This is used to tag methods for TODOs, having many of these is a bad
sign...

	self flag: #Bob
"
Class {
	#name : 'FlagIconStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-Breakpoints',
	#package : 'Reflectivity-Tools',
	#tag : 'Breakpoints'
}

{ #category : 'testing' }
FlagIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'defaults' }
FlagIconStyler >> highlightColor [

	^ self theme warningBackgroundColor alpha: 0.1
]

{ #category : 'defaults' }
FlagIconStyler >> iconFor: aNode [
	^ self iconNamed: #flag
]

{ #category : 'testing' }
FlagIconStyler >> shouldStyleNode: aNode [
	^aNode isMessage and: [aNode selector = #flag:]
]
