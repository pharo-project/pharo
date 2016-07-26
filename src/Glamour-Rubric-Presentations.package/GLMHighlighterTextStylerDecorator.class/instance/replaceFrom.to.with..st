editing
replaceFrom: start to: stop with: aText
	self okToStyle
		ifFalse: [ ^ next replaceFrom: start to: stop with: aText ].
	self
		disableDrawingWhile: [
			aText addAttribute: self defaultFontChange.
			text ifNil: [ text := self text ].
			text replaceFrom: start to: stop with: (self styler format: aText).
			self style: text.
			next text: text.
			self recomposeFrom: start to: start + aText size - 1 delta: aText size - (stop - start + 1).
			self textArea paragraphWasComposedFrom: start to: start + aText size - 1.
			self textArea paragraphReplacedTextFrom: start to: stop with: aText ]