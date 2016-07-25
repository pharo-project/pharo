joystick
joystickOn: index

	^ (((self primReadJoystick: index) bitShift: -27) bitAnd: 1) ~= 0
	