Extension { #name : #SHRBTextStyler }

{ #category : #'*Deprecated11' }
SHRBTextStyler >> font: aFont [

	self deprecated: 'This variable is never used and was removed.'
]

{ #category : #'*Deprecated11' }
SHRBTextStyler class >> initialTextAttributesForPixelHeight: aNumber [

	self deprecated: 'Use #initialTextAttributes instead.' transformWith: '`@rcv initialTextAttributesForPixelHeight: `@arg1' -> '`@rcv initialTextAttributes'.
	^ self initialTextAttributes
]

{ #category : #'*Deprecated11' }
SHRBTextStyler class >> initialTextAttributesForPixelHeight: aNumber styleTable: aStyleTable [

	self
		deprecated: 'Use #initialTextAttributesForStyleTable: instead.'
		transformWith: '`@rcv initialTextAttributesForPixelHeight: `@arg1 styleTable: `arg2' -> '`@rcv initialTextAttributesForStyleTable: `@arg2'.
	^ self initialTextAttributesForStyleTable: aStyleTable
]
