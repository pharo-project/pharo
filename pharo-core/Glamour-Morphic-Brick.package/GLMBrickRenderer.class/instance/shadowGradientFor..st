private-shadow
shadowGradientFor: aBrick

	^ GradientFillStyle ramp: { 0.0 -> (aBrick shadowColor) . 1.0 -> (aBrick shadowColor alpha: 0)}