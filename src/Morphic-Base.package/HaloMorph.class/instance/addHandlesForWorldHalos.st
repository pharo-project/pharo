private
addHandlesForWorldHalos
	"Add handles for world halos, like the man said"

	| box w |
	w := self world ifNil:[target world].
	self removeAllMorphs.  "remove old handles, if any"
	self bounds: target bounds.
	box := w bounds insetBy: 9.
	target addWorldHandlesTo: self box: box.

	self addNameBeneath: (box insetBy: (0@0 corner: 0@10)) string: innerTarget externalName.
	growingOrRotating := false.
	self layoutChanged.
	self changed.
