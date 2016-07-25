rendering
renderHighlight: anInterval with: paint on: canvas

	| renderer |
	renderer := TxHighlightRenderer highlighting: anInterval in: self with: paint.
	renderer renderHighlightOn: canvas.