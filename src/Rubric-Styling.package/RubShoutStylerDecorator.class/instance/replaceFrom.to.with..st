editing
replaceFrom: start to: stop with: aText
	self okToStyle
		ifFalse: [ ^ next replaceFrom: start to: stop with: aText ].
		
	self paragraph disableDrawingWhile: [
		
		aText addAttribute: self paragraph defaultFontChange.
		text ifNil: [ text := self text ].
		text replaceFrom: start to: stop with: (self styler format: aText).
		replaceStart := start.
		replaceStop := stop.
		self style: text.
		next text: text.
		
		self paragraph ifNotNil: [ :paragraph |
			paragraph recomposeFrom: start to: start + aText size - 1 delta: aText size - (stop - start + 1).
			paragraph textArea paragraphWasComposedFrom: start to: start + aText size - 1.
			paragraph textArea paragraphReplacedTextFrom: start to: stop with: aText 
		]
	
	]