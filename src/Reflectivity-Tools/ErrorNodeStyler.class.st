Class {
	#name : 'ErrorNodeStyler',
	#superclass : 'IconStyler',
	#category : 'Reflectivity-Tools-ErrorAndSemanticStyling',
	#package : 'Reflectivity-Tools',
	#tag : 'ErrorAndSemanticStyling'
}

{ #category : 'styling' }
ErrorNodeStyler >> addIconStyle: aNode from: start to: stop color: aColor [

	(self shouldStyleNode: aNode) ifFalse: [ ^ self ].
	
	renderer addDecoration: (IconStylerDecoration new
		name: self stylerName;
		interval: (start to: stop);
		highlightColor: aColor;
		icon: (self iconFor: aNode);
		iconBlock: (self iconBlock: aNode);
		title: (self iconLabel: aNode);
		underline: true;
		underlineColor: Color red;
		yourself)
]

{ #category : 'defaults' }
ErrorNodeStyler >> highlightColor [

	^ Color gray alpha: 0.1
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
			 underline: true;
			 underlineColor: Color red;
			 icon: (self iconFor: anEnglobingNode);
			 iconBlock: (self iconBlock: anEnglobingNode);
			 title: (self iconLabel: anEnglobingNode);
			 yourself).
	anEnglobingNode contents do: [ :each | self visitNode: each ]
]
