geometry updating
adjustBottomY
	| heights  bottomY verticalSize |
	heights := self lines collect: [ :each | each lineHeight ].
	verticalSize := heights sum.
	bottomY := self container top + verticalSize.
	self container: (self container withBottom: bottomY truncated).