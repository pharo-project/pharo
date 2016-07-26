private
setColor: aColor toHandle: aHandle 
	"private - change the color to the given handle, applying the 
	alternate look if corresponds"
	aHandle color: aColor.
	self gradientHalo
		ifTrue: [| fill | 
			fill := GradientFillStyle ramp: {0.0 -> aColor muchLighter. 1.0 -> aColor darker}.
			fill origin: aHandle topLeft.
			fill direction: aHandle extent.
			aHandle fillStyle: fill] 