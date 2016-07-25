There is only one instance of me, Display. It is a global and is used to handle general user requests to deal with the whole display screen. 
	Although I offer no protocol, my name provides a way to distinguish this special instance from all other Forms. This is useful, for example, in dealing with saving and restoring the system.
	To change the depth of your Display...
		Display newDepth: 16.
		Display newDepth: 8.
		Display newDepth: 1.
Valid display depths are 1, 2, 4, 8, 16 and 32.  It is suggested that you run with your monitors setting the same, for better speed and color fidelity.  Note that this can add up to 4Mb for the Display form.  Finally, note that newDepth: ends by executing a 'ControlManager restore' which currently terminates the active process, so nothing that follows in the doit will get executed.

Depths 1, 2, 4 and 8 bits go through a color map to put color on the screen, but 16 and 32-bit color use the pixel values directly for RGB color (5 and 8 bits per, respectivlely).  The color choice an be observed by executing Color fromUser in whatever depth you are using.
