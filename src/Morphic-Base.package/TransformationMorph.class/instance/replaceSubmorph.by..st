submorphs-add/remove
replaceSubmorph: oldMorph by: newMorph
	| t b |
	t := transform.
	b := bounds.
	super replaceSubmorph: oldMorph by: newMorph.
	transform := t.
	bounds := b.
	self layoutChanged