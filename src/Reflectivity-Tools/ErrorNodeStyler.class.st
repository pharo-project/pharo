Class {
	#name : #ErrorNodeStyler,
	#superclass : #IconStylerRealTime,
	#category : #'Reflectivity-Tools-RealTimeStyling'
}

{ #category : #styling }
ErrorNodeStyler >> addIconStyle: aNode from: start to: stop color: aColor [
	|conf|
	(self shouldStyleNode: aNode) ifFalse: [ ^self ].
	conf := RubConfigurationChange new.
	conf configurationBlock: [ :text| |r| 
		r := RubUnderlinedSegmentMorph from: aNode start to: aNode stop + 1.
		text addSegment: r.
		
		r label: (self iconLabelBlock: aNode).
		r icon: (self iconFor: aNode).
		r iconBlock: (self iconBlock: aNode).
		r color: aColor.
		r borderColor: self borderColor.
	].

	textModel announce: conf.
]

{ #category : #defaults }
ErrorNodeStyler >> borderColor [ 
	^Color red
]

{ #category : #'as yet unclassified' }
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

{ #category : #visiting }
ErrorNodeStyler >> shouldStyleNode: aNode [
	^ aNode isParseError
]
