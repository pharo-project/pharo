private
openAddedJoystick: index
	| joystick |
	joystick := SDL2 joystickOpen: index.
	JoystickMap at: index put: joystick.
