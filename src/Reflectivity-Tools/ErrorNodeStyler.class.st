Class {
	#name : #ErrorNodeStyler,
	#superclass : #IconStyler,
	#category : #'Reflectivity-Tools-ErrorAndSemanticStyling'
}

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

{ #category : #visiting }
ErrorNodeStyler >> visitEnglobingErrorNode: anEnglobingNode [

	| conf |
	conf := RubConfigurationChange new.
	conf configurationBlock: [ :text | 
		| r |
		r := self segmentMorphClass from: anEnglobingNode stop to: anEnglobingNode stop + 1.
		text addSegment: r.
		r label: (self iconLabelBlock: anEnglobingNode).
		r icon: (self iconFor: anEnglobingNode).
		r iconBlock: (self iconBlock: anEnglobingNode).
		r color: self highlightColor.
		r borderColor: self borderColor ].
	textModel announce: conf.

	anEnglobingNode content do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
ErrorNodeStyler >> visitUnreachableStatement: anUnreachableStatement [
	"self overrides visitEnglobingErrorNode:"
	^ super visitEnglobingErrorNode: anUnreachableStatement
]
