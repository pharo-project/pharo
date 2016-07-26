test-attribute
testApplyFontOnOneSpan
	|font|
	self newText: 'AAACCC'.
	font := TxFontAttribute with: #fakeFont.
	
	selection all.
	
	selection applyAttribute:  font.
	"use start position because there are one span "
	self assert: (selection start span getAttribute: TxFontAttribute) equals: #fakeFont.
	