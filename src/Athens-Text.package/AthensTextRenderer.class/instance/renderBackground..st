as yet unclassified
renderBackground: bkObj
"self halt."
	canvas setPaint: bkObj color.
	canvas drawShape: ( bkObj start@0 corner: bkObj start + bkObj width @ currentLine heightAndDescent)
