"
I'm in charge to style  the editor for  message sends with the ""halt"", ""halt:"" or ""haltIf:"" selectors, like ""self halt"".
"
Class {
	#name : 'HaltIconStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-Breakpoints',
	#package : 'Reflectivity-Tools',
	#tag : 'Breakpoints'
}

{ #category : 'testing' }
HaltIconStyler class >> isStaticStyler [

	^ true
]

{ #category : 'defaults' }
HaltIconStyler >> highlightColor [

	^ self theme dangerBackgroundColor alpha: 0.1
]

{ #category : 'defaults' }
HaltIconStyler >> iconFor: aNode [
	^ self iconNamed: #halt
]

{ #category : 'defaults' }
HaltIconStyler >> iconLabelBlock: aNode [
	^ [aNode selector]
]

{ #category : 'testing' }
HaltIconStyler >> shouldStyleNode: aNode [
	| selectorsToBeHighlighted |
	selectorsToBeHighlighted := { 'halt'. 'halt:'. 'haltIf:'. 'haltIfNil' }.
	^aNode isMessage and: [ selectorsToBeHighlighted anySatisfy: [ :sel| sel = aNode selector ]]
]
