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
SpMorphStyleTest >> testNestedStyles [
	| lookup labelAdapter styleSheet |

	styleSheet := (SpStyleSTONReader fromString: '
.application [
	.label [
		Geometry { #width: 21 },
		.nested [ 
			Geometry { #width: 42 },
			.deepNested [ 
				Geometry { #width: 84 } ] ] ] ]').

	"No nesting"
	labelAdapter := SpLabelPresenter new 
		buildWithSpec;
		adapter.
	lookup := SpMorphStyle fromStylesheet: styleSheet adapter: labelAdapter.
	self assert: lookup width equals: 21.

	"nesting"			
	labelAdapter := SpLabelPresenter new 
		addStyle: 'nested';
		buildWithSpec;
		adapter.
	lookup := SpMorphStyle fromStylesheet: styleSheet adapter: labelAdapter.
	self assert: lookup width equals: 42.
	
	"deep-nesting"			
	labelAdapter := SpLabelPresenter new 
		addStyle: 'nested.deepNested';
		buildWithSpec;
		adapter.
	lookup := SpMorphStyle fromStylesheet: styleSheet adapter: labelAdapter.
	self assert: lookup width equals: 84
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
