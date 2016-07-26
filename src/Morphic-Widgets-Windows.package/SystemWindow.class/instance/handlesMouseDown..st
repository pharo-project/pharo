events
handlesMouseDown: evt 
	"If I am not the topWindow, then I will only respond to dragging by the title bar.
	Any other click will only bring me to the top"
	
	^ self isEmbedded not