test-attribute
testApplyFontWithOneSplit
	"We can have only one or two split apply on a TextModel "
	|color |
	self newText: 'AAACCC'.
	self assert: text spans size = 3. "<< AAACCC >>"
	color := TxBackColorAttribute with: #foo.

	selection start moveToStart.
	selection end moveToStart moveRight: 3.

	selection applyAttribute: color.
	self assert: text spans size equals: 4.
	cursor moveToStart.
	self assert: (cursor span style get: TxBackColorAttribute) equals: #foo.
	cursor moveToEnd.
	self assert: (cursor span style get: TxBackColorAttribute) equals: (TxBackColorAttribute defaultValue).