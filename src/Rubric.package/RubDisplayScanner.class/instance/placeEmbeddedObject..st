scanning
placeEmbeddedObject: anchoredMorph
	anchoredMorph relativeTextAnchorPosition ifNotNil:[
		anchoredMorph position: 
			anchoredMorph relativeTextAnchorPosition +
			(anchoredMorph owner bounds origin x @ 0)
			- (0@morphicOffset y) + (0@lineY).
		^true
	].
	(super placeEmbeddedObject: anchoredMorph) ifFalse: [^ false].
	anchoredMorph isMorph ifTrue: [
		anchoredMorph position: ((destX - anchoredMorph width)@lineY) - morphicOffset
	] ifFalse: [
		destY := lineY.
		baselineY := lineY + anchoredMorph height..
		runX := destX.
		anchoredMorph 
			displayOn: bitBlt destForm 
			at: destX - anchoredMorph width @ destY
			clippingBox: bitBlt clipRect
			rule: Form blend
			fillColor: Color white 
	].
	^ true