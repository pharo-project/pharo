misc
swapBuffers: aWindow
	self checkThread.
	SDL2 glSwapWindow: aWindow handle.