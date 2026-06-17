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

{ #category : 'visiting' }
ErrorNodeStyler >> shouldStyleNode: aNode [
	^ aNode isError and: [ aNode errorNotices isNotEmpty ]
]

{ #category : 'private' }
ErrorNodeStyler >> stylerName [

	^ 'error'
]

{ #category : 'visiting' }
ErrorNodeStyler >> visitEnglobingErrorNode: anEnglobingNode [

	(self shouldStyleNode: anEnglobingNode) ifFalse: [ ^ super visitEnglobingErrorNode: anEnglobingNode ].
	renderer addDecoration: (IconStylerDecoration new
			 name: self stylerName;
			 interval: (anEnglobingNode errorPosition to: anEnglobingNode errorPosition);
			 highlightColor: self highlightColor;
			 isUnderline: true;
			 underlineColor: Color red;
			 icon: (self iconFor: anEnglobingNode);
			 iconBlock: (self iconBlock: anEnglobingNode);
			 title: (self iconLabel: anEnglobingNode);
			 yourself).
	anEnglobingNode contents do: [ :each | self visitNode: each ]
]
