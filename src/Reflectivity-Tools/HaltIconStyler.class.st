"
I'm in charge to style  the editor for  message sends with the ""halt"", ""halt:"" or ""haltIf:"" selectors, like ""self halt"".
"
Class {
	#name : #HaltIconStyler,
	#superclass : #IconStyler,
	#category : #'Reflectivity-Tools-Breakpoints'
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
	^ [aNode selector]
]

{ #category : #testing }
HaltIconStyler >> shouldStyleNode: aNode [
	| selectorsToBeHighlighted |
	selectorsToBeHighlighted := { 'halt'. 'halt:'. 'haltIf:'. 'haltIfNil' }.
	^aNode isMessage and: [ selectorsToBeHighlighted anySatisfy: [ :sel| sel = aNode selector ]].
]
