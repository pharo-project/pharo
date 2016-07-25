menu
toggleSmoothing

	smoothCurve := smoothCurve not.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds