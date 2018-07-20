Extension { #name : #Margin }

{ #category : #'*NewMethodFinder' }
Margin class >> approvedSelectors [

	^ #(asMargin bottom expandRectangle: extendRectangle: fromNumber: fromPoint: fromRectangle: height
	    insetRectangle isZero left right setTop:left:bottom:right: top width)
]
