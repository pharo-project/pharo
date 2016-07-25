test-attribute
testApplySameFont
	"I made this test because for other attribute like bold if you apply this attribute when it already on the span the attribute is removed "
	| font |
	self newText: 'AAACCC'.
	font := TxFontAttribute with: #fakeFont.
	
	selection all.
	
	selection applyAttribute: font.
	"use start position because there are one span "
	self assert: (selection start span style get: TxFontAttribute) equals: #fakeFont.
	
	selection applyAttribute:  font.
	
	self assert: (selection start span style get: TxFontAttribute) equals: #fakeFont.