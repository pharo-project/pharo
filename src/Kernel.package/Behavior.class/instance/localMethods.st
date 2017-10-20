accessing method dictionary
localMethods
	"returns the methods of classes excluding the ones of the traits that the class uses" 
	 
	^ self methods select: [:each | self includesLocalSelector: each selector].