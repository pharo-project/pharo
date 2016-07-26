rendering
athensFillPath: aPath on: anAthensCanvas

	anAthensCanvas surface engine
		prepareForRendering;
		registerFormFill: form origin: origin direction: direction normal: self normal;
		fillBezierShape: aPath contoursForFilling