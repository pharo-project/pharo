filein/out
localMethods
	"returns the methods of classes including the ones of the traits that the class uses" 
	 
	^ self methods select: [:each | self includesLocalSelector: each selector].