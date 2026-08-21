Extension { #name : 'RubParagraphDecorator' }

{ #category : '*Athens-Morphic' }
RubParagraphDecorator >> drawOnAthensCanvas: aCanvas bounds: aRectangle color: aColor [

	(self canDrawDecoratorsOn: aCanvas) ifFalse: [ ^ self ].

	self drawOnAthensCanvas: aCanvas.
	self next drawOnAthensCanvas: aCanvas bounds: aRectangle color: aColor
]
