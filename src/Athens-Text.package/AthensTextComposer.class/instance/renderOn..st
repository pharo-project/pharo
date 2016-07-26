as yet unclassified
renderOn: aCanvas

	| renderer |
	
	renderer := AthensTextRenderer new.
	renderer render: text lines: lines on: aCanvas.
