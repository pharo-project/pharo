initialization
initializeSlider
	"Make the slider raised."
	
	self 
		setSlider: (( BracketMorph newBounds: self totalSliderArea)
						horizontal: self bounds isWide;
						color: self thumbColor;
						borderStyle: (BorderStyle raised baseColor: Color white; width: 1))
		 withShadow: ((BracketMorph newBounds: self totalSliderArea)
						horizontal: self bounds isWide;
						color: self pagingArea color;
						borderStyle: (BorderStyle inset baseColor: (Color white alpha: 0.6); width: 1)).