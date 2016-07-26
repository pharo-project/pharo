selected
selectedFontStyleIndex
	| family member |
	selectedFontStyleIndex ifNotNil: [
		^selectedFontStyleIndex := selectedFontStyleIndex min: self fontStyleList size].
	family := self fontList at: selectedFontIndex ifAbsent:[^0].
	member := family closestMemberWithStretchValue: stretchValue weightValue: weightValue slantValue: slantValue.
	selectedFontStyleIndex := self fontStyleList indexOf: member.
	^selectedFontStyleIndex