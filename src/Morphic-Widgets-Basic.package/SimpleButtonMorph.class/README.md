I am labeled, rectangular morph which allows the user to click me. I can be configured to send my "target" the message "actionSelector" with "arguments" when I am clicked. I may have a label, implemented as a StringMorph.

Example:

	SimpleButtonMorph new
		target: Beeper;
		label: 'Beep!';
		actionSelector: #beep; 
		openInWorld

Structure:
instance var 	Type		Description 
target 			Object 		The Object to notify upon a click 
actionSelector 	Symbol 		The message to send to Target (#messageName) 
arguments 		Array 		Arguments to send with #actionSelection (optional) 
actWhen 		Symbol 		When to take action: may be #buttonUp (default), #buttonDown,
								#whilePressed, or #startDrag 
oldColor 		Color 		Used to restore color after click 

Another example: a button which quits the image without saving it.

	SimpleButtonMorph new
		target: Smalltalk;
		label: 'quit';
		actionSelector: #snapshot:andQuit:;
		arguments: (Array with: false with: true); 
		openInWorld

