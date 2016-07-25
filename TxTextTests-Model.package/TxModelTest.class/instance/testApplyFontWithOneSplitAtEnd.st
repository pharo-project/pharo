test-attribute
testApplyFontWithOneSplitAtEnd
	"We can have only one or two split apply on a TextModel "
	| color originalColor |
	self newText: 'AAACCC'.
	originalColor := text defaultStyle get: TxBackColorAttribute.
	color := TxBackColorAttribute with: #foo.
	selection := text newSelection.
	selection start moveRight: 3.

	selection applyAttribute: color.
	
	self assert: text spans size equals: 4.
	
	cursor moveToEnd.
	self assert: (cursor span style get: TxBackColorAttribute) equals: #foo.

	cursor moveToStart.
	self assert: (cursor span style get: TxBackColorAttribute) equals: originalColor.