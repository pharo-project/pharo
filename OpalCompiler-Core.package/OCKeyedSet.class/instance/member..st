adding
member: newObject
 	"Include newObject as one of the receiver's elements, if already exists just return it"
 
 	| index |
 	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
 	index := self findElementOrNil: (keyBlock value: newObject).
 	(array at: index) ifNotNil: [^ array at: index].
 	self atNewIndex: index put: newObject.
 	^ newObject