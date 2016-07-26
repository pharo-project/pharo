*Polymorph-Widgets
treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph

	"Method extracted from LazyListMorph"
	| attrs useDefaultFont f itemBounds |
	
	attrs := self attributesAt: 1.
	useDefaultFont := true.
	attrs
		do: [:att | att
				forFontInStyle: TextStyle default
				do: [:fon | useDefaultFont := false]].
	f := useDefaultFont
				ifTrue: [ aMorph font ]
				ifFalse: [ self fontAt: 1 withStyle: TextStyle default ].
	itemBounds := drawBounds  withHeight: f height.
	itemBounds := itemBounds align: itemBounds leftCenter with: drawBounds  leftCenter.
	"center the item if the font height is different to that of our font"
	aCanvas
		drawString: self
		in: itemBounds
		font: (f emphasized: (self emphasisAt: 1))
		color: drawColor 
		underline: ((self emphasisAt: 1)
				bitAnd: 4)
				> 0
		underlineColor: drawColor 
		strikethrough: ((self emphasisAt: 1)
				bitAnd: 16)
				> 0
		strikethroughColor: drawColor 