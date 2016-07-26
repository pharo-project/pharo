enumerating
children
	"Answer an array containing references to the direct children of this reference."
	| reference |
	reference := self resolve.
	^ (reference fileSystem childNamesAt: reference path) 
		collect: [ :aName | self / aName ]