instance creation
swipeLeft

	^ GLMAnimation new
		linear;
		duration: 250;
		from: 0;
		to: -1.2;
		animation: [ :b :v | b changed. b translateXTo: (v * b width). b changed ]