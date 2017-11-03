"
I'm in charge to style an ast when there are metalinks
"
Class {
	#name : #MetalinkIconStyler,
	#superclass : #IconStyler,
	#category : #Reflectivity-Examples
}

{ #category : #options }
MetalinkIconStyler class >> shouldStyle [
	^false
]

{ #category : #defaults }
MetalinkIconStyler >> iconBlock: aNode [
	^[ aNode inspect]
]

{ #category : #defaults }
MetalinkIconStyler >> iconFor: aNode [
	^ self iconNamed: #arrowUpIcon.
]

{ #category : #defaults }
MetalinkIconStyler >> iconLabel: aNode [
	^ 'Metalink'
]

{ #category : #testing }
MetalinkIconStyler >> shouldStyleNode: aNode [
	^aNode hasMetalink
]
