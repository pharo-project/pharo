Extension { #name : 'ImageMorph' }

{ #category : '*Athens-Morphic' }
ImageMorph >> drawOnAthensCanvas: aCanvas [

	| cached style|

	cached := aCanvas cacheAt: formSet ifAbsentPut: [
		formSet asForm asAthensPaintOn: aCanvas.
		].

	aCanvas setPaint: cached.

		aCanvas paintTransform restoreAfter: [
			aCanvas paintTransform translateBy: self innerBounds origin "negated".

			aCanvas drawShape: self innerBounds.
		].
	(style := self borderStyle) ifNotNil: [
		aCanvas setStrokePaint: style.
		aCanvas drawShape: bounds]
]
