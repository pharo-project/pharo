Class {
	#name : 'ErrorNodeStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-ErrorAndSemanticStyling',
	#package : 'Reflectivity-Tools',
	#tag : 'ErrorAndSemanticStyling'
}

{ #category : 'defaults' }
ErrorNodeStyler >> highlightColor [
	^nil
]

{ #category : 'defaults' }
ErrorNodeStyler >> iconFor: aNode [
	^ self iconNamed: #error
]

{ #category : 'defaults' }
ErrorNodeStyler >> iconLabel: aNode [

	self flag: 'What if more than one error?'.
	^ aNode errorNotices first messageText
]

{ #category : 'hooks' }
ErrorNodeStyler >> segmentMorphClass [

	^ RubUnderlinedSegmentMorph
]

{ #category : 'visiting' }
ErrorNodeStyler >> shouldStyleNode: aNode [
	^ aNode isError and: [ aNode errorNotices isNotEmpty ]
]

{ #category : 'visiting' }
ErrorNodeStyler >> visitEnglobingErrorNode: anEnglobingNode [

	| conf |

	(self shouldStyleNode: anEnglobingNode) ifFalse: [ ^ super visitEnglobingErrorNode: anEnglobingNode ].

	"Only add red underline at the error position (instead on the whole node)"
	conf := RubConfigurationChange new.
	conf configurationBlock: [ :text |
		| r |
		r := self segmentMorphClass from: anEnglobingNode errorPosition to: anEnglobingNode errorPosition.
		text addSegment: r.
		r label: (self iconLabelBlock: anEnglobingNode).
		r icon: (self iconFor: anEnglobingNode).
		r iconBlock: (self iconBlock: anEnglobingNode).
		r color: self highlightColor.
		r borderColor: self borderColor ].
	textModel announce: conf.

	anEnglobingNode contents do: [ :each | self visitNode: each ]
]
