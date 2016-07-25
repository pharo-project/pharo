*Morphic-Base-Widgets
listRenderOn: aCanvas atRow: aRow bounds: bounds color: color backgroundColor: backgroundColor from: aMorph
	"Method extracted from LazyListMorph"
	| attrs useDefaultFont f itemBounds |
	
	attrs := self attributesAt: 1.
	useDefaultFont := true.
	attrs
		do: [:att | att
				forFontInStyle: aMorph font textStyle
				do: [:fon | useDefaultFont := false]].
	f := useDefaultFont
				ifTrue: [ aMorph font]
				ifFalse: [ self fontAt: 1 withStyle: aMorph font textStyle ].
	itemBounds := bounds withHeight: f height.
	itemBounds := itemBounds align: itemBounds leftCenter with: bounds leftCenter.
	"center the item if the font height is different to that of our font"
	aCanvas
		drawString: self
		in: itemBounds
		font: (f emphasized: (self emphasisAt: 1))
		color: color
		underline: ((self emphasisAt: 1)
				bitAnd: 4)
				> 0
		underlineColor: color
		strikethrough: ((self emphasisAt: 1)
				bitAnd: 16)
				> 0
		strikethroughColor: color