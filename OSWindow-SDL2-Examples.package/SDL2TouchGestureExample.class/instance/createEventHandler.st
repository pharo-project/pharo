factory
createEventHandler
	^OSWindowGestureEventHandler new
				touchExample: self;
				registerGesture: OSTouchTwoFingersScrollDetector new;
				registerGesture: OSTouchTwoFingersPinchDetector new;
				yourself