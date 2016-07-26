action
unemphasizedSelectedFont
	|name font family |
	family := self fontList at: self selectedFontIndex ifAbsent:[nil].
	family ifNil:[^nil].
	(family isKindOf: TextStyleAsFontFamily)
		ifTrue:[^family textStyle fontOfPointSize: pointSize].
	name :=  family familyName.
	font := LogicalFont 
		familyName: name
		pointSize: pointSize 
		stretchValue: 5
		weightValue: 400
		slantValue: 0.
	font realFont isTTCFont "true for FreeTypeFont"
		ifFalse: [font := font textStyle fontOfPointSize: pointSize].
	^font
