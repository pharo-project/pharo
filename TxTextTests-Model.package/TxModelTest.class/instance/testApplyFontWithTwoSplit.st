test-attribute
testApplyFontWithTwoSplit
	"We can have only one or two split apply on a TextModel "
	| color default |
	self newText: 'AAACCC'.
	self assert: text spans size equals: 3.
	color := TxBackColorAttribute with: #foo.
	default := text defaultStyle get: TxBackColorAttribute.
	
	selection beSelection end moveLeft: 2.
	selection start moveLeft: 4.

	selection applyAttribute: color.

	self assert: text spans size equals: 5.

	cursor moveToStart.
	self assert: (cursor span style get: TxBackColorAttribute) equals: default.

	cursor moveRight: 3.
	self assert: (cursor span style get: TxBackColorAttribute) equals: #foo.

	cursor moveRight: 3.
	self assert: (cursor span style get: TxBackColorAttribute) equals: default.