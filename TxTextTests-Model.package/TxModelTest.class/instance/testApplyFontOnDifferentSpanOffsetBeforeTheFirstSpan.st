test-attribute
testApplyFontOnDifferentSpanOffsetBeforeTheFirstSpan
	"We can have only one or two split apply on a TextModel "
	|color font  |
	self newText: 'AAA','CCC'.
	self assert: text spans size = 3.
	color := TxBackColorAttribute new.
	font := TxFontAttribute with: #something.
	selection end moveLeft: 2.
	selection start moveLeft: 4.
	selection applyAttribute:  color.
	selection all.
	selection start moveRight.
	selection applyAttribute: font.
	cursor moveToStart.
	self assert: text spans size = 6.
	self assert: ((cursor  span  style) get: TxFontAttribute) equals: (TxFontAttribute defaultValue).
	cursor moveRight:2.
	self assert: ((cursor  span  style) get: TxFontAttribute) equals:  #something.
	cursor moveRight.
	self assert: ((cursor  span  style) get: TxFontAttribute) equals:  #something.
	cursor moveRight: 3.
	self assert: ((cursor  span  style) get: TxFontAttribute) equals:  #something.