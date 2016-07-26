comparing
hasEqualElements: otherCollection
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	(otherCollection isKindOf: SequenceableCollection) ifFalse: [^ false].
	(size := self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].
	^ true