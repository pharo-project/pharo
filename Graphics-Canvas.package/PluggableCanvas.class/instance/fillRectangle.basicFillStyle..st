drawing-rectangles
fillRectangle: aRectangle basicFillStyle: aFillStyle
	"Fill the given rectangle with the given, non-composite, fill style."

	| pattern |

	(aFillStyle isKindOf: InfiniteForm) ifTrue: [
		^self infiniteFillRectangle: aRectangle fillStyle: aFillStyle
	].

	aFillStyle isSolidFill ifTrue:[ ^self fillRectangle: aRectangle color: aFillStyle asColor].

	"We have a very special case for filling with infinite forms"
	(aFillStyle isBitmapFill and:[aFillStyle origin = (0@0)]) ifTrue:[
		pattern := aFillStyle form.
		(aFillStyle direction = (pattern width @ 0) 
			and:[aFillStyle normal = (0@pattern height)]) ifTrue:[
				"Can use an InfiniteForm"
				^self fillRectangle: aRectangle color: (InfiniteForm with: pattern)].
	].
	"Use a BalloonCanvas instead"
	self balloonFillRectangle: aRectangle fillStyle: aFillStyle.
