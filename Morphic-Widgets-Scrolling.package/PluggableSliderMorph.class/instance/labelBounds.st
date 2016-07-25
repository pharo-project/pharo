drawing
labelBounds
	| deltaY deltaX labelWidth treshold bnd |
	bnd := self innerBounds.
	labelWidth := self font widthOfStringOrText: self label.
	deltaY := (self height - self font height) / 2.
	treshold := bnd left + self labelGap + labelWidth.
	deltaX := (slider left < treshold or: [ sliderShadow visible and: [ sliderShadow left < treshold ] ])
		ifTrue: [ bnd width - self labelGap - labelWidth ]
		ifFalse: [ self labelGap ].
	^ bnd translateBy: deltaX @ deltaY