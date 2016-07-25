accessing
aaTransform
	"Return a transformation for the current anti-aliasing level"
	| matrix |
	matrix := MatrixTransform2x3 withScale: (self aaLevel) asFloat asPoint.
	matrix offset: (self aaLevel // 2) asFloat asPoint.
	^matrix composedWith:(MatrixTransform2x3 withOffset: destOffset asFloatPoint)