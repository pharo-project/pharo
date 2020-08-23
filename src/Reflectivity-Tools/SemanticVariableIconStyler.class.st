Class {
	#name : #SemanticVariableIconStyler,
	#superclass : #IconStyler,
	#category : #'Reflectivity-Tools-ErrorAndSemanticStyling'
}

{ #category : #defaults }
SemanticVariableIconStyler >> borderColor [ 
	^Color red
]

{ #category : #defaults }
SemanticVariableIconStyler >> highlightColor [
	^(Color red alpha: 0.2)
]

{ #category : #defaults }
SemanticVariableIconStyler >> iconFor: aNode [
	^ self iconNamed: #warning
]

{ #category : #defaults }
SemanticVariableIconStyler >> iconLabel: aNode [
	^ aNode name printString, ' is not a known variable '.
]

{ #category : #hooks }
SemanticVariableIconStyler >> segmentMorphClass [

	^ RubTextBackgroundColorSegmentMorph
]

{ #category : #testing }
SemanticVariableIconStyler >> shouldStyleNode: aNode [
	^ aNode isVariable and: [ aNode binding isUndeclaredVariable ]
]
