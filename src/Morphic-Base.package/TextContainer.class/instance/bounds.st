private
bounds
	| bounds theText |
	self fillsOwner ifFalse: [^ textMorph textBounds].
	theText := textMorph.
	bounds := theText owner innerBounds.
	bounds := bounds insetBy: (textMorph valueOfProperty: #margins ifAbsent: [1@1]).
	theText owner submorphsBehind: theText do:
		[:m | bounds := bounds merge: m fullBounds].
	^ bounds