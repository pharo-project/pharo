private
translucentMaskFor: alphaValue depth: d
	"Return a pattern representing a mask usable for stipple transparency"
	^(self translucentPatterns at: d) at: ((alphaValue min: 1.0 max: 0.0) * 4) rounded + 1 