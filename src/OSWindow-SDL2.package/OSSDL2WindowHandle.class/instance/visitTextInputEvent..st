event handling
visitTextInputEvent: event
	| osEvent |
	
	osEvent := OSTextInputEvent for: osWindow.
	osEvent text: (ZnUTF8Encoder new decodeBytes: event text);
			position: self mousePosition.
			
	self convertButtonState: SDL2 mouseState modState: SDL2 modState modifiers: osEvent modifiers.
	^ osEvent deliver.