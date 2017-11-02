"
I'm in charge to style  the editor for  a ""self halt"".
"
Class {
	#name : #HaltIconStyler,
	#superclass : #IconStyler,
	#category : 'Reflectivity-Tools-Breakpoints'
}

{ #category : #defaults }
HaltIconStyler >> highlightColor [
	^(Color red alpha: 0.1)
]

{ #category : #defaults }
HaltIconStyler >> iconFor: aNode [
	^ self iconNamed: #haltIcon 
]

{ #category : #defaults }
HaltIconStyler >> iconLabelBlock: aNode [
	^ ['halt']
]

{ #category : #testing }
HaltIconStyler >> shouldStyleNode: aNode [
	^aNode isMessage and: [ aNode selector = 'halt']
]
