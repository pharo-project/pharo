instance creation
borderColor

	^ GLMAnimation new
		step: [ :brick :from :to :delta |
			|r g b a|
			r := (to red - from red) * delta + from red.
			g := (to green - from green) * delta + from green.
			b := (to blue - from blue) * delta + from blue.
			a := (to alpha - from alpha) * delta + from alpha.
			Color r: r g: g b: b alpha: a ];
		animation: [ :b :v | b borderColor: v. b changed ]