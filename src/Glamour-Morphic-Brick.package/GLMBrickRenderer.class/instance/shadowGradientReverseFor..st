private-shadow
shadowGradientReverseFor: aBrick

	^ GradientFillStyle ramp: { 0.0 -> (aBrick shadowColor alpha: 0). 1.0 -> (aBrick shadowColor) }