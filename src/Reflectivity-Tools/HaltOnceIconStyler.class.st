"
I'm in charge to style  the editor for  a ""haltOnce"". Clicking on it re-enables the haltOnce.
"
Class {
	#name : #HaltOnceIconStyler,
	#superclass : #HaltIconStyler,
	#category : 'Reflectivity-Tools-Breakpoints'
}

{ #category : #defaults }
HaltOnceIconStyler >> iconBlock: aNode [
	^[	aNode removeProperty: #Once ifAbsent: [aNode propertyAt: #Once put: true ]]
]

{ #category : #defaults }
HaltOnceIconStyler >> iconLabelBlock: aNode [
	^ [(aNode hasProperty: #Once) 
		ifTrue: ['inactive, click to turn on']
		ifFalse: ['active, click to turn off' ]]
]

{ #category : #testing }
HaltOnceIconStyler >> shouldStyleNode: aNode [
	^aNode isMessage and: [ aNode selector = #haltOnce]
]
