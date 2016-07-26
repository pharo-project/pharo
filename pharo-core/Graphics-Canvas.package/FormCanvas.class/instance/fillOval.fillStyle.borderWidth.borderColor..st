drawing-ovals
fillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc
	"Fill the given oval."

	self flag: #bob.		"this and its siblings could be moved up to Canvas with the
						right #balloonFillOval:..."

	(aFillStyle isBitmapFill and:[aFillStyle isKindOf: InfiniteForm]) ifTrue:[
		self flag: #fixThis.
		^self fillOval: aRectangle color: aFillStyle borderWidth: bw borderColor: bc].
	(aFillStyle isSolidFill) ifTrue:[
		^self fillOval: aRectangle color: aFillStyle asColor borderWidth: bw borderColor: bc].
	"Use a BalloonCanvas instead"
	self balloonFillOval: aRectangle fillStyle: aFillStyle borderWidth: bw borderColor: bc