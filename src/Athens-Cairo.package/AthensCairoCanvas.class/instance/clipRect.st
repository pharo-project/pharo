accessing
clipRect
	^ currentClipRect ifNil: [ 0 @ 0 extent: self surface extent ]