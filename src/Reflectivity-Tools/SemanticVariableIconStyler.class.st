Class {
	#name : #SemanticVariableIconStyler,
	#superclass : #IconStylerRT,
	#category : #'Reflectivity-Tools-RealTimeStyling'
}

{ #category : #styling }
SemanticVariableIconStyler >> addIconStyle: aNode from: start to: stop color: aColor [
	|conf|
	(self shouldStyleNode: aNode) ifFalse: [ ^self ].
	conf := RubConfigurationChange new.
	conf configurationBlock: [ :text| |r| 
		r := RubTextBackgroundColorSegmentMorph from: aNode start to: aNode stop + 1.
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

{ #category : #testing }
SemanticVariableIconStyler >> shouldStyleNode: aNode [
	^ aNode isVariable and: [ aNode binding isUndeclaredVariable ]
]
