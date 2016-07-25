adding
add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject"

	| index element |
	index := self scanFor: newObject.
	((element := array at: index) == flag or: [ element == nil ])
		ifTrue: [self atNewIndex: index put: newObject asSetElement].
	^newObject