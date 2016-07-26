text area font
fontName: fontName size: pointSize
	| newTextStyle |
	(LogicalFontManager current allFamilies detect: [ :f | f familyName = fontName ] ifNone: [  ])
		ifNotNil: [ :fam | 
			| style font |
			style := fam members detect: [ :m | m styleName = 'Regular' ].
			font := style asLogicalFontOfPointSize: pointSize.
			newTextStyle := font textStyle ]
		ifNil: [ newTextStyle := ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy ].
	self textArea privateTextStyle: newTextStyle.
	self addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfSize: pointSize))