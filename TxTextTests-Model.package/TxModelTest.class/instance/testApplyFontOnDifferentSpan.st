test-attribute
testApplyFontOnDifferentSpan
	"We can have only one or two split apply on a TextModel "
	|color font |
	self newText: 'AAACCC'.
	self assert: text spans size = 3.
	color := TxBackColorAttribute with: Color red.
	font := TxFontAttribute with: #something.
	selection end moveLeft: 2.
	selection start moveLeft: 4.
	selection applyAttribute:  color.
	selection all.
	selection applyAttribute: font.
	cursor moveToStart.
	self assert: text spans size = 5.
	self assert: ((cursor  span  style) get: TxFontAttribute) equals: #something.
	cursor moveRight:3.
	self assert: ((cursor  span  style) get: TxFontAttribute) equals: #something.
	cursor moveRight:3.
	self assert: ((cursor  span  style) get: TxFontAttribute) equals: #something.