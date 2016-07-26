drawing-rectangles
infiniteFillRectangle: aRectangle fillStyle: aFillStyle

	| additionalOffset rInPortTerms clippedPort targetTopLeft clipOffset ex |

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset := 0@0.
	ex := aFillStyle form extent.
	rInPortTerms := (aRectangle intersect: aFillStyle boundingBox ifNone: ["nothing to draw" ^ self ]) translateBy: origin.
	clippedPort := port clippedBy: rInPortTerms.
	targetTopLeft := clippedPort clipRect topLeft truncateTo: ex.
	clipOffset := rInPortTerms topLeft - targetTopLeft.
	additionalOffset := (clipOffset \\ ex) - ex.
	^aFillStyle
		displayOnPort: clippedPort
		offsetBy: additionalOffset
