"
I'm in charge to style  the editor for  a ""haltOnCount:"". Clicking on it resets the counter.
"
Class {
	#name : #HaltOnCountIconStyler,
	#superclass : #HaltIconStyler,
	#category : #'Reflectivity-Tools-Breakpoints'
}

{ #category : #defaults }
HaltOnCountIconStyler >> iconBlock: aNode [
	^[ aNode removeProperty: #haltCount ifAbsent: []]
]

{ #category : #defaults }
HaltOnCountIconStyler >> iconLabelBlock: aNode [
	^ [(aNode hasProperty: #haltCount) 
		ifTrue: [(aNode propertyAt: #haltCount) asString, ' ,click to reset']
		ifFalse: ['count: 0' ]]
]

{ #category : #testing }
HaltOnCountIconStyler >> shouldStyleNode: aNode [
	^ aNode isMessage
		and: [ #(#haltOnCount: #haltFromCount:) includes: aNode selector ]
]
