factory
createEventHandler
	^ OSWindowGestureEventHandler new
			touchExample: self;
			registerGesture: OSTouchTwoFingersScrollDetector new;
			registerGesture: OSTouchTwoFingersPinchDetector new;
			registerGesture: OSTouchThreeFingersSwipeDetector new;
			registerGesture: OSTouchTwoFingersRotationDetector new;
			registerGesture: OSTouchOneFingerDoubleTapDetector new;
			registerGesture: OSTouchOneFingerHoldDetector new;
			yourself