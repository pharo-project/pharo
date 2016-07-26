displaying
displayOnPort: aPort at: offset

	| targetBox patternBox savedMap top left |

	self flag: #bob.

	"this *may* not get called at the moment. I have been trying to figure out the right way for this to work and am using #displayOnPort:offsetBy: as my current offering - Bob"

	(patternForm isForm) ifFalse: [
		"patternForm is a Pattern or Color; just use it as a mask for BitBlt"
		^ aPort fill: aPort clipRect fillColor: patternForm rule: Form over].

	"do it iteratively"
	targetBox := aPort clipRect.
	patternBox := patternForm boundingBox.
	savedMap := aPort colorMap.
	aPort sourceForm: patternForm;
		fillColor: nil;
		combinationRule: Form paint;
		sourceRect: (0@0 extent: patternBox extent);
		colorMap: (patternForm colormapIfNeededFor: aPort destForm).
	top := (targetBox top truncateTo: patternBox height) "- (offset y \\ patternBox height)".
	left :=  (targetBox left truncateTo: patternBox width) "- (offset x \\ patternBox width)".
	left to: (targetBox right - 1) by: patternBox width do:
		[:x | top to: (targetBox bottom - 1) by: patternBox height do:
			[:y | aPort destOrigin: x@y; copyBits]].
	aPort colorMap: savedMap.
