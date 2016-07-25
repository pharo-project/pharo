label-animation
animateToNormal

	self borderAnimation
		from: self borderColor first;
		to: (self borderColor first alpha: 0);
		animate: self