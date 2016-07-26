private clip transform
canvasClipBy: aClipRect during: aBlock
	| oldRect |
	oldRect := self clipRect.
	currentClipRect := aClipRect.
	[ self canvas clipBy: currentClipRect during:[ aBlock value ]]
		ensure: [ currentClipRect := oldRect ]