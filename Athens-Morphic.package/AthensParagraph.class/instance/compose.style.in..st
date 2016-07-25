as yet unclassified
compose: aText style: aTextStyle in: rect 

	text := aText.
	textStyle := aTextStyle.
	containerRect := rect.
	lines := (AthensTextComposer scan: aText for: AthensTextScanner new) lines.