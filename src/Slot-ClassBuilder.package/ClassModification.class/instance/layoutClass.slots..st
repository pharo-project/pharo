accessing
layoutClass: layoutClass slots: slotArray
	| parentLayout newScope newLayout |
	parentLayout := self superLayout.
	newScope := parentLayout slotScope extend: slotArray.
	newLayout := layoutClass extending: parentLayout scope: newScope host: self target.
	self layout: newLayout