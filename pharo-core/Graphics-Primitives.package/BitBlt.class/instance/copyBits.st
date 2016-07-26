copying
copyBits
	"Primitive. Perform the movement of bits from the source form to the 
	destination form. Fail if any variables are not of the right type (Integer, 
	Float, or Form) or if the combination rule is not implemented. 
	In addition to the original 16 combination rules, this BitBlt supports
	16	fail (to simulate paint)
	17	fail (to simulate mask)
	18	sourceWord + destinationWord
	19	sourceWord - destinationWord
	20	rgbAdd: sourceWord with: destinationWord
	21	rgbSub: sourceWord with: destinationWord
	22	rgbDiff: sourceWord with: destinationWord
	23	tallyIntoMap: destinationWord
	24	alphaBlend: sourceWord with: destinationWord
	25	pixPaint: sourceWord with: destinationWord
	26	pixMask: sourceWord with: destinationWord
	27	rgbMax: sourceWord with: destinationWord
	28	rgbMin: sourceWord with: destinationWord
	29	rgbMin: sourceWord bitInvert32 with: destinationWord
"
	"Check for compressed source, destination or halftone forms"
	<primitive: 'primitiveCopyBits' module: 'BitBltPlugin'>
	(combinationRule between: 30 and: 31) ifTrue: 
		[ "No alpha specified -- re-run with alpha = 1.0"
		^ self copyBitsTranslucent: 255 ].
	(sourceForm isForm and: [ sourceForm unhibernate ]) ifTrue: [ ^ self copyBits ].
	(destForm isForm and: [ destForm unhibernate ]) ifTrue: [ ^ self copyBits ].
	(halftoneForm isForm and: [ halftoneForm unhibernate ]) ifTrue: [ ^ self copyBits ].

	"Check for unimplmented rules"
	combinationRule = Form oldPaint ifTrue: [ ^ self paintBits ].
	combinationRule = Form oldErase1bitShape ifTrue: [ ^ self eraseBits ].

	"Check if BitBlt doesn't support full color maps"
	(colorMap notNil and: [ colorMap isColormap ]) ifTrue: 
		[ colorMap := colorMap colors.
		^ self copyBits ].
	"Check if clipping gots us way out of range"
	self clipRange ifTrue: 
		[ self roundVariables.
		^ self copyBitsAgain ].
	self error: 'Bad BitBlt arg (Fraction?); proceed to convert.'.
	"Convert all numeric parameters to integers and try again."
	self roundVariables.
	^ self copyBitsAgain