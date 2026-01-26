"
A styler that highlight with green all executed statement
"
Class {
	#name : 'CoverageRubricStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Examples',
	#package : 'Reflectivity-Examples'
}

{ #category : 'styling' }
CoverageRubricStyler >> addIconStyle: aNode from: start to: stop [
	|rejectClass|

	rejectClass := { OCMethodNode . OCSequenceNode }.
	(rejectClass includes: aNode class) ifTrue: [ ^self ]. "do nothing"
	aNode hasBeenExecuted ifTrue: [  super addIconStyle: aNode from: start to: stop]
]

{ #category : 'defaults' }
CoverageRubricStyler >> iconBlock: aNode [
	^[ aNode inspect]
]

{ #category : 'defaults' }
CoverageRubricStyler >> iconFor: aNode [
	^ self iconNamed: #arrowUp
]

{ #category : 'defaults' }
CoverageRubricStyler >> iconLabel: aNode [
	^ 'Metalink'
]

{ #category : 'testing' }
CoverageRubricStyler >> shouldStyleNode: aNode [
	^aNode hasMetalink
]
