halo handling
removeHaloFromClick: anEvent on: aMorph 
	| halo |
	halo := self halo
				ifNil: [^ self].
	(halo target hasOwner: self)
		ifTrue: [^ self].
	(halo staysUpWhenMouseIsDownIn: aMorph)
		ifFalse: [self removeHalo]