geometry
expandSlider
	"Compute the new size of the slider (use the old sliderThickness as a minimum)."
	| r |
	r := self totalSliderArea.
	slider extent: (bounds isWide
		ifTrue: [((r width * interval) asInteger max: self sliderThickness) @ slider height]
		ifFalse: [slider width @ ((r height * interval) asInteger max: self sliderThickness)])