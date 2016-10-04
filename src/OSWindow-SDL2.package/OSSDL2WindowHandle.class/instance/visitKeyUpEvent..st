event handling
visitKeyUpEvent: event
	| osEvent keysym |
	keysym := event keysym.
	osEvent := OSKeyUpEvent for: osWindow.
	osEvent scanCode: keysym scancode;
			symbol: keysym sym;
			repeat: event repeat;
			position: self mousePosition.
			
	self convertButtonState: SDL2 mouseState modState: keysym mod modifiers: osEvent modifiers.
	^ osEvent deliver.