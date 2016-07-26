private
image: aForm at: aPoint sourceRect: sourceRect rule: rule
	| warp dstRect srcQuad dstOffset |
	(self ifNoTransformWithIn: sourceRect) & false
		ifTrue:[^super image: aForm at: aPoint sourceRect: sourceRect rule: rule].
	dstRect := (transform localBoundsToGlobal: (aForm boundingBox translateBy: aPoint)).
	dstOffset := 0@0. "dstRect origin."
	"dstRect := 0@0 corner: dstRect extent."
	srcQuad := transform globalPointsToLocal: (dstRect innerCorners).
	srcQuad := srcQuad collect:[:pt| pt - aPoint].
	warp := (WarpBlt current toForm: form)
			sourceForm: aForm;
			cellSize: 2;  "installs a new colormap if cellSize > 1"
			combinationRule: Form over.
	warp copyQuad: srcQuad toRect: (dstRect translateBy: dstOffset).

	self frameRectangle: (aForm boundingBox translateBy: aPoint) color: Color green.

	"... TODO ... create a bitmap fill style from the form and use it for a simple rectangle."