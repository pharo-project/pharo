submorphs-accessing
changeColumnDisplay
	self paragraphProvider columnRuler
		ifNil: [ self paragraphProvider withColumns ]
		ifNotNil: [ self paragraphProvider withoutColumns ].
	self textArea changed.
	self updateContents