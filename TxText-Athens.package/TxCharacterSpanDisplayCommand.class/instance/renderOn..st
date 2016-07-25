rendering
renderOn: renderer

	renderer setFont: (span getAttribute: TxFontAttribute).
	renderer setColor: (span getAttribute: TxForeColorAttribute).
	
	renderer renderCharacters: span characters from: start to: stop.
	renderer advance:advance.