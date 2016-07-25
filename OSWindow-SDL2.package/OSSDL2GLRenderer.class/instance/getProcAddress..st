misc
getProcAddress: procName
	self checkThread.
	^ SDL2 glGetProcAddress: procName