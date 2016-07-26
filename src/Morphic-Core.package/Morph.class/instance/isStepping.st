stepping and presenter
isStepping
	"Return true if the receiver is currently stepping in its world"
	| aWorld |
	^ (aWorld := self world)
		ifNil:		[false]
		ifNotNil:	[aWorld isStepping: self]