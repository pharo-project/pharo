testing
isMethodDefinedInImage
	"answer whether the method represented by the receiver is present in the image"
	
	^ self type = #method
		and: [self classIncludesSelector]