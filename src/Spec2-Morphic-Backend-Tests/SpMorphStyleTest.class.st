Class {
	#name : #SpMorphStyleTest,
	#superclass : #TestCase,
	#category : #'Spec2-Morphic-Backend-Tests'
}

{ #category : #private }
SpMorphStyleTest >> newLookup: aString [
	| rootClass |

	rootClass := SpStyleSTONReader fromString: aString.
	^ SpMorphStyle newStyles: rootClass flattenClasses 
]

{ #category : #tests }
SpMorphStyleTest >> testApplyTo [
	| styleResolver morph |
	
	styleResolver := self newLookup: '.application [ 
		Geometry { #width: 11 },
		.other [
			Geometry { #width: 22 },
			.deep1 [
				Geometry { #width: 33 }
			],
			.deep2 [
				Geometry { #width: 42 }
			]
		]
	]'.

	morph := Morph new.
	styleResolver applyTo: morph.
	self assert: morph width equals: 42
]

{ #category : #tests }
SpMorphStyleTest >> testDeepLookup [
	"This should answer the deepest width"
	| styleResolver |
	
	styleResolver := self newLookup: '.application [ 
		Geometry { #width: 11 },
		.other [
			Geometry { #width: 22 },
			.deep [
				Geometry { #width: 42 }
			]
		]
	]'.

	self assert: styleResolver width equals: 42
]

{ #category : #tests }
SpMorphStyleTest >> testDeepMultipleLookup [
	"This should answer the deepest width (last appearence)"
	| styleResolver |
	
	styleResolver := self newLookup: '.application [ 
		Geometry { #width: 11 },
		.other [
			Geometry { #width: 22 },
			.deep1 [
				Geometry { #width: 33 }
			],
			.deep2 [
				Geometry { #width: 42 }
			]
		]
	]'.

	self assert: styleResolver width equals: 42
]

{ #category : #tests }
SpMorphStyleTest >> testFromStylesheetAdapter [
	| lookup labelAdapter |

	labelAdapter := SpLabelPresenter new 
		buildWithSpec;
		adapter.

	lookup := SpMorphStyle
		fromStylesheet: (SpStyleSTONReader fromString: '
.application [
	.label [
		Geometry { #width: 42 }
		]
	]')
		adapter: labelAdapter.
		
	self assert: lookup width equals: 42.
	
	lookup := SpMorphStyle
		fromStylesheet: (SpStyleSTONReader fromString: '
.application [
	Geometry { #width: 42 },
	.label [ ]
	]')
		adapter: labelAdapter.
	self assert: lookup width equals: 42.
	
	"it refuses deeper class (because is not part of the tree)"
	lookup := SpMorphStyle
		fromStylesheet: (SpStyleSTONReader fromString: '
.application [
	.label [
		Geometry { #width: 42 }
	],
	.other [ 
		.label [ 
			Geometry { #width: 22 } 
		] 
	]
]')
		adapter: labelAdapter.

]

{ #category : #tests }
SpMorphStyleTest >> testSimpleLookup [
	"this should answer the width defined in the only level"
	| styleResolver |
	
	styleResolver := self newLookup: '.application [ 
		Geometry { #width: 42 } 
	]'.

	self assert: styleResolver width equals: 42
]
