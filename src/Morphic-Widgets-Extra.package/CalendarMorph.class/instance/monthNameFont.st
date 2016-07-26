accessing
monthNameFont

	| font boldItalic |
	font := LogicalFont familyName: 'Bitmap DejaVu Sans' pointSize: 12.
	boldItalic := TextEmphasis italic emphasisCode | TextEmphasis bold emphasisCode.
	^ font emphasis: boldItalic.