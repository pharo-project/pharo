adding
add: newObject
	"Include newObject as one of the receiver's elements.  If equivalent is already present don't add and return equivalent object"

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: newObject.
	^ (array at: index) 
		ifNil: [self atNewIndex: index put: newObject. newObject]
		ifNotNil: [array at: index]