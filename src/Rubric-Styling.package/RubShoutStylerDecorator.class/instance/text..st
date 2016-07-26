accessing
text: aText
	self okToStyle
		ifFalse: [ ^ next text: aText ].
	self
		disableDrawingWhile: [
			aText addAttribute: self defaultFontChange.
			next text: (text := (self styler format: aText)).
			replaceStart := 1.
			replaceStop := text size.
			self style: text]