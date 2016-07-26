instance creation
newFrom: aSimilarObject
	"Create an object that has similar contents to aSimilarObject. If the classes have any instance variables with the same names, copy them across. If this is bad for a class, override this method."

	^ (self isVariable
		ifTrue: [self basicNew: aSimilarObject basicSize]
		ifFalse: [self basicNew]) copySameFrom: aSimilarObject