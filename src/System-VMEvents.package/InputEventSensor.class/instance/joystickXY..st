joystick
joystickXY: index

	| inputWord x y |
	inputWord := self primReadJoystick: index.
	x := (inputWord bitAnd: 16r7FF) - 16r400.
	y := ((inputWord bitShift: -11) bitAnd: 16r7FF) - 16r400.
	^ x@y
	