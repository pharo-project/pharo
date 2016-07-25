displaying
displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger fillColor: aForm
	"This is the real display message, but it doesn't get used until the new
	display protocol is installed."
	| targetBox patternBox bb |
	(patternForm isForm) ifFalse:
		[^ aDisplayMedium fill: clipRectangle rule: ruleInteger fillColor: patternForm].

	"Do it iteratively"
	targetBox := aDisplayMedium boundingBox intersect: clipRectangle ifNone: [ ^ self ].
	patternBox := patternForm boundingBox.
	bb := BitBlt destForm: aDisplayMedium sourceForm: patternForm fillColor: aForm
		combinationRule: ruleInteger destOrigin: 0@0 sourceOrigin: 0@0
		extent: patternBox extent clipRect: clipRectangle.
	bb colorMap:
		(patternForm colormapIfNeededFor: aDisplayMedium).
	(targetBox left truncateTo: patternBox width)
		to: targetBox right - 1 by: patternBox width do:
		[:x |
		(targetBox top truncateTo: patternBox height)
			to: targetBox bottom - 1 by: patternBox height do:
			[:y |
			bb destOrigin: x@y; copyBits]]