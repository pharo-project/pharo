navigating
parent
	| size parent |
	self isRoot ifTrue: [^ self].
	self isAllParents ifTrue: [^ self / '..'].
	
	size := self size - 1.
	parent := self class new: size.
	1 to: size do: [:i | parent at: i put: (self at: i)].
	^ parent