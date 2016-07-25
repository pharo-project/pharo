private
transformVerticesFrom: oldOwner to: newOwner
	| oldTransform newTransform world newVertices |
	world := self world.
	oldTransform := oldOwner
		ifNil: [ IdentityTransform new ]
		ifNotNil: [ oldOwner transformFrom: world ].
	newTransform := newOwner
		ifNil: [ IdentityTransform new ]
		ifNotNil: [ newOwner transformFrom: world ].
	newVertices := vertices collect: [ :ea | newTransform globalPointToLocal:
		(oldTransform localPointToGlobal: ea) ].
	self setVertices: newVertices.
