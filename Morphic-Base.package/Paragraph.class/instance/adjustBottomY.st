private
adjustBottomY
	| heights  bottomY verticalSize |
	heights := lines collect:[:each| each lineHeight ].
	verticalSize := heights sum.
	bottomY := container top + verticalSize.
	container := container withBottom: bottomY.