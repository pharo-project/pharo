halo handling
removeHaloAround: aMorph
	"remove the receiver's halo associated to aMorph (if any)"
	| halo |
	halo := self halo.
	halo
		ifNil: [ ^ self ].
	halo target == aMorph
		ifFalse: [ ^ self ].
	self removeProperty: #halo.
	halo delete.