"
I'm in charge to style  the editor for  `halt:` messages, like `self halt: 'an error happened'`
"
Class {
	#name : #HaltColonIconStyler,
	#superclass : #HaltIconStyler,
	#category : #'Reflectivity-Tools-Breakpoints'
}

{ #category : #defaults }
HaltColonIconStyler >> iconLabelBlock: aNode [
	^ ['halt:']
]

{ #category : #testing }
HaltColonIconStyler >> shouldStyleNode: aNode [
	^aNode isMessage and: [ aNode selector = 'halt:']
]
