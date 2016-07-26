grabbing
aboutToBeGrabbedBy: aHand
	"Don't allow the receiver to act outside a Menu"
	| menu box |
	(owner notNil and:[owner hasSubmorphs ]) ifTrue:[
		"I am a lonely menuitem already; just grab my owner"
		owner stayUp: true.
		^owner aboutToBeGrabbedBy: aHand].
	box := self bounds.
	menu := UIManager default newMenuIn: self for: nil.
	menu addMorphFront: self.
	menu bounds: box.
	menu stayUp: true.
	self isSelected: false.
	^menu