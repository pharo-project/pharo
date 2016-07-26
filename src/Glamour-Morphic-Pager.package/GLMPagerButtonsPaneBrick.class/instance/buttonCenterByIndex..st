buttons-calculations
buttonCenterByIndex: aNumber
	| width |
	width := self pagerModel circleButtonSize.
	^ ( ( aNumber - 1 ) * ( width ) ) + ( width / 2 ).