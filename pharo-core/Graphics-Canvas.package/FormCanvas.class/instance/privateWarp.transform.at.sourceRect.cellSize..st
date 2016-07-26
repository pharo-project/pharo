private
privateWarp: aForm transform: aTransform at: extraOffset sourceRect: sourceRect cellSize: cellSize
	"Warp the given using the appropriate transform and offset."
	| globalRect sourceQuad warp tfm |
	tfm := aTransform.
	globalRect := tfm localBoundsToGlobal: sourceRect.
	sourceQuad := (tfm sourceQuadFor: globalRect) collect:[:p| p - sourceRect topLeft].
	extraOffset ifNotNil:[globalRect := globalRect translateBy: extraOffset].
     warp := (WarpBlt current toForm: port destForm)
                combinationRule: Form paint;
                sourceQuad: sourceQuad destRect: (globalRect origin corner: globalRect corner+(1@1));
                clipRect: port clipRect.
	warp cellSize: cellSize.
	warp sourceForm: aForm.
	warp warpBits