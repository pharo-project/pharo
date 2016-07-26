event handling
convertButtonState: mouseState modState: modState modifiers: modifiers
	| shift ctrl alt gui |
	modifiers buttons button1: (mouseState bitAnd: SDL_BUTTON_LMASK) ~= 0.
	modifiers buttons button2: (mouseState bitAnd: SDL_BUTTON_MMASK) ~= 0.
	modifiers buttons button3: (mouseState bitAnd: SDL_BUTTON_RMASK) ~= 0.
	modifiers buttons button4: (mouseState bitAnd: SDL_BUTTON_X1MASK) ~= 0.
	modifiers buttons button5: (mouseState bitAnd: SDL_BUTTON_X2MASK) ~= 0.

	shift := (modState bitAnd: KMOD_SHIFT) ~= 0.
	ctrl := (modState bitAnd: KMOD_CTRL) ~= 0.
	alt := (modState bitAnd: KMOD_LALT) ~= 0.
	gui := (modState bitAnd: KMOD_GUI) ~= 0.
	modifiers leftShift: shift; rightShift: shift;
			   leftCtrl: ctrl; rightCtrl: ctrl;
			   leftAlt: alt; rightAlt: alt;
				leftCmd: gui; rightCmd: gui.