private
executeNotifying: aBlock 
	| undos |
	self changeClass rename: newName.
	undos := changes collect: [ :each | 
		(each renameChangesForClass: oldName asSymbol to: newName asSymbol) 
			executeNotifying: aBlock ].
	^ self copy
		changes: undos reverse;
		rename: newName to: oldName;
		yourself