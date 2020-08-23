Class {
	#name : #ErrorNodeStyler,
	#superclass : #IconStyler,
	#category : #'Reflectivity-Tools-ErrorAndSemanticStyling'
}

{ #category : #defaults }
ErrorNodeStyler >> borderColor [ 
	^Color red
]

{ #category : #'for demo' }
ErrorNodeStyler >> demo [
	"statements mes; 2; isNil".
]

{ #category : #defaults }
ErrorNodeStyler >> highlightColor [
	^nil
]

{ #category : #defaults }
ErrorNodeStyler >> iconFor: aNode [
	^ self iconNamed: #error
]

{ #category : #defaults }
ErrorNodeStyler >> iconLabel: aNode [
	^ aNode errorMessage.
]

{ #category : #hooks }
ErrorNodeStyler >> segmentMorphClass [

	^ RubUnderlinedSegmentMorph
]

{ #category : #visiting }
ErrorNodeStyler >> shouldStyleNode: aNode [
	^ aNode isParseError
]
