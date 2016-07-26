as yet unclassified
renderBackground: aColor width: w
"self halt."
	canvas setPaint: aColor.
	canvas drawShape: ( advance x@0 corner: advance x + w +1@ currentLine heightAndDescent)
