selected
selectedFontIndex: anIndex

	| family member newStyleIndex |
	anIndex = 0 ifTrue: [^self].
	selectedFontIndex := anIndex.
	"change the selected style to be the closest to the last
	user selected weight slant and stretch values. 
	By user selected I mean that the user changed the style list selection, 
	rather than a change being forced because a particular family didn't have that style"
	family := self fontList at: selectedFontIndex.
	member := family closestMemberWithStretchValue: stretchValue weightValue: weightValue slantValue: slantValue.
	newStyleIndex := self fontStyleList indexOf: member.
	selectedFontStyleIndex := newStyleIndex. 
	self setPointSizeListFrom: member.
	self announcer announce: #selectedFontStyleIndex.
	self announcer announce: #selectedFontIndex.
	