private
openAddedController: index
	| controller |
	controller := SDL2 gameControllerOpen: index.
	JoystickMap at: index put: controller.
	