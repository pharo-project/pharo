classes and traits
allClasses  
	"Return all the class defines in the Smalltalk SystemDictionary"

	^ self classNames collect: [:name | self at: name]