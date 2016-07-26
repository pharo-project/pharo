other
warpFrom: sourceQuad toRect: destRect
	^ (WarpBlt toForm: port destForm)
		combinationRule: Form paint;
		sourceQuad: sourceQuad destRect: (destRect translateBy: origin);
		clipRect: clipRect