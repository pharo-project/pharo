accessing
gradientLook
	^ gradientLook ifNil: [gradientLook := self theme currentSettings preferGradientFill]