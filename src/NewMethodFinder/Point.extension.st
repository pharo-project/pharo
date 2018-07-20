Extension { #name : #Point }

{ #category : #'*NewMethodFinder' }
Point class >> approvedSelectors [

	^ #(x y * + - / // \\ abs max min reciprocal < <= > >= closeTo: max: min: min:max:
	    adaptToCollection:andSend: adaptToNumber:andSend: asFloatPoint asIntegerPoint asMargin asNonFractionalPoint
	    asPoint
	    isInsideCircle:with:with: sideOf: to:intersects:to: to:sideOf: triangleArea:with: interpolateTo:at:
	    crossProduct: directionToLineFrom:to: dotProduct: eightNeighbors flipBy:centerAt: fourDirections fourNeighbors
	    grid: insideTriangle:with:with: leftRotated nearestPointAlongLineFrom:to: nearestPointOnLineFrom:to:
	    octantOf: quadrantOf: reflectedAbout: rightRotated sign sortsBefore: squareDistanceTo: transposed
	    corner: extent: rectangle: isFloatPoint isIntegerPoint isPoint isPoint isZero adhereTo: negated scaleBy:
	    scaleFrom:to: scaleTo: translateBy: ceiling floor roundDownTo: roundTo: roundUpTo: rounded truncateTo:
	    truncated angle angleWith: bearingToPoint: degrees dist: distanceTo: normal normalized onLineFrom:to:
	    onLineFrom:to:within: rotateBy:about: rotateBy:centerAt: scaledByDisplayScaleFactor setR:degrees: theta
	   )
]
