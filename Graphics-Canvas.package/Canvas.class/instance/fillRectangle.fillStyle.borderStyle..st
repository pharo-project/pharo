drawing-rectangles
fillRectangle: aRectangle fillStyle: aFillStyle borderStyle: aBorderStyle
	"Fill the given rectangle."
       aFillStyle isTransparent ifFalse: [
               self fillRectangle: (aRectangle insetBy: aBorderStyle width) fillStyle: aFillStyle ].
       
       (aBorderStyle notNil and: [aBorderStyle width]) >= 0 ifTrue: [ aBorderStyle frameRectangle: aRectangle on: self ].
       