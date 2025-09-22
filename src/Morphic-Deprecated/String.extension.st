Extension { #name : 'String' }

{ #category : '*Morphic-Deprecated' }
String >> heightToDisplayInTree: aTree [

	^ aTree fontToUse height
]

{ #category : '*Morphic-Deprecated' }
String >> treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph [

	aCanvas
		drawString: self
		in: drawBounds
		font: aMorph font
		color: drawColor
]

{ #category : '*Morphic-Deprecated' }
String >> widthToDisplayInTree: aTree [

	^ aTree fontToUse widthOfStringOrText: self contents
]
