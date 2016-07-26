saving
saveImageInNewContext

	" This method is used for saving the Smalltalk image in a new process so the current process with all stack information may be forgotten.
	
	Do not use this method for common image saving. This method is designated for saving the image during the shrinking process in headless images. "

	[ self privSaveImageWithCleanup ] forkAt: 40.