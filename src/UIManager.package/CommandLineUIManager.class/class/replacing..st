instance creation
replacing: aUIManager

	"Replace the current UI manager with instance of myself. 
	Keep a backup reference to old manager, and then restore it, when image will be interactive again. "
	
	^ self new replacing: aUIManager