adding
add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	index := self scanFor: newObject.
	(array at: index) ifNil: [self atNewIndex: index put: newObject asSetElement].
	^ newObject