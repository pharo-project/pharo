Class {
	#name : #SemanticMessageIconStyler,
	#superclass : #IconStyler,
	#category : #'Reflectivity-Tools-ErrorAndSemanticStyling'
}

{ #category : #styling }
SemanticMessageIconStyler >> addIconStyle: aNode from: start to: stop color: aColor [
	|conf|
	(self shouldStyleNode: aNode) ifFalse: [ ^self ].
	conf := RubConfigurationChange new.
	conf configurationBlock: [ :text| |r| 
		r := RubTextBackgroundColorSegmentMorph from: aNode start to: aNode stop + 1.
		text addSegment: r.
		r color: aColor.
		r borderColor: self borderColor.
		r label: (self iconLabelBlock: aNode).
		r icon: (self iconFor: aNode).
		r iconBlock: (self iconBlock: aNode).
	].

	textModel announce: conf.
]

{ #category : #defaults }
SemanticMessageIconStyler >> borderColor [ 
	^Color red
]

{ #category : #defaults }
SemanticMessageIconStyler >> highlightColor [
	^(Color lightOrange alpha: 0.2)
]

{ #category : #defaults }
SemanticMessageIconStyler >> iconFor: aNode [
	^ self iconNamed: #smallQuestion
]

{ #category : #defaults }
SemanticMessageIconStyler >> iconLabel: aNode [
	^ aNode selector value asString, ' is not a recognized method '.
]

{ #category : #testing }
SemanticMessageIconStyler >> shouldStyleNode: aNode [
	^ aNode isMessage and: [ (Symbol findInternedSelector: aNode selector value asString) isNil ]
]
