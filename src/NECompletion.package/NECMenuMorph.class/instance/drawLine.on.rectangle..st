drawing
drawLine: index on: aCanvas rectangle: rectangle
	| symbol font type string preString model highlightRectangle |
	model := context model.
	symbol := model entries at: index.
	string := symbol contents.
	preString := string asLowercase findString: context completionToken asLowercase.
	preString := preString <= 0
		ifFalse: [ string first: preString - 1 ]
		ifTrue: [ String empty ].
	type := symbol type.
	font := self selectFont: type.
	index = self selected
		ifTrue: [ 
			| rect |
			rect := rectangle withBottom: rectangle top + self class itemHeight.
			aCanvas fillRectangle: rect color: self class scrollColor.
			self detailPosition: rect topRight ].
	highlightRectangle := rectangle translateBy: (font widthOfString: preString) @ 0.
	highlightRectangle := highlightRectangle withWidth: (font widthOfString: context completionToken).
	aCanvas 
		clipBy: rectangle 
		during: [ :c | c fillRectangle: highlightRectangle color: (Color gray alpha: 0.3) ].
	aCanvas
		drawString: string
		in: (rectangle insetBy: 1)
		font: font
		color: (self selectColor: type)