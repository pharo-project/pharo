Class {
	#name : 'SemanticMessageIconStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-ErrorAndSemanticStyling',
	#package : 'Reflectivity-Tools',
	#tag : 'ErrorAndSemanticStyling'
}

{ #category : 'defaults' }
SemanticMessageIconStyler >> borderColor [
	^Color red
]

{ #category : 'defaults' }
SemanticMessageIconStyler >> highlightColor [

	^ Color lightOrange alpha: 0.2
]

{ #category : 'defaults' }
SemanticMessageIconStyler >> iconFor: aNode [
	^ self iconNamed: #smallQuestion
]

{ #category : 'defaults' }
SemanticMessageIconStyler >> iconLabel: aNode [
	^ aNode selector value asString, ' is not a recognized method '
]

{ #category : 'hooks' }
SemanticMessageIconStyler >> segmentMorphClass [

	^ RubTextBackgroundColorSegmentMorph
]

{ #category : 'testing' }
SemanticMessageIconStyler >> shouldStyleNode: aNode [
	^ aNode isMessage and: [ aNode selector value isSelectorSymbol not ]
]
