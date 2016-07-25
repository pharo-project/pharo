initialization
initializeImagesCache
	"initialize the receiver's ImagesCache. 
	 
	normally this method is not evaluated more than in the class 
	initializazion. "

	" 
	ScrollBar initializeImagesCache.
	"

	ArrowImagesCache := self theme createArrowImagesCache.
	BoxesImagesCache := self theme createBoxImagesCache