settings
gradientButtonLook 
	^ UseGradientLook ifNil: [UseGradientLook := self theme settings preferGradientFill]