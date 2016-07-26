label-animation
animateToEditFocused

	self borderAnimation
		from: self borderColor first;
		to: self themer focusedBorderColor;
		animate: self