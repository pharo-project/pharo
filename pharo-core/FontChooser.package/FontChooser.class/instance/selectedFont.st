selected
selectedFont
	| font style |

	font := self unemphasizedSelectedFont.
	font ifNil:[^nil].
	style := self fontStyleList at: self selectedFontStyleIndex ifAbsent:[nil].
	style ifNil:[^nil].
	(style isKindOf: TextStyleAsFontFamilyMember)
		ifTrue:[	^font emphasized: style emphasisCode].		
	^LogicalFont 
		familyName: font familyName 
		pointSize: pointSize 
		stretchValue: style stretchValue
		weightValue: style weightValue
		slantValue: style slantValue

	