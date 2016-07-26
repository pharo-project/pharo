selected
selectedFontStyleIndex: anIndex
	| familyMember |
	anIndex = 0
		ifTrue: [^self].
	selectedFontStyleIndex := anIndex.
	familyMember := self fontStyleList at: anIndex.
	self setStyleValuesFrom: familyMember.
	self announcer announce:  #selectedFontStyleIndex.