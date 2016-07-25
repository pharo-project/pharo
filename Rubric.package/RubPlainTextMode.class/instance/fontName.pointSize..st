text area font
fontName: fontName pointSize: pointSize
	| newTextStyle |
	(LogicalFontManager current allFamilies detect: [ :f | f familyName = fontName ] ifNone: [  ])
		ifNotNil: [ :fam | 
			| style font |
			style := fam members detect: [ :m | m styleName = 'Regular' ].
			font := style asLogicalFontOfPointSize: pointSize.
			newTextStyle := font textStyle ]
		ifNil: [ newTextStyle := ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy ].
	self textArea privateTextStyle: newTextStyle.
	self textArea addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfPointSize: pointSize))