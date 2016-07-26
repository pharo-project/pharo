grabbing
duplicateMorph: evt
	"Make and return a duplicate of the receiver's argument"
	| dup menu |
	dup := self duplicate isSelected: false.
	menu := UIManager default newMenuIn: self for: nil.
	menu addMorphFront: dup.
	menu bounds: self bounds.
	menu stayUp: true.
	evt hand grabMorph: menu from: owner. "duplicate was ownerless so use #grabMorph:from: here"
	^menu