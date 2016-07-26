test-attribute
testApplyColor
	| color |
	self newText: 'AAACCC'.
	color := TxBackColorAttribute with: Color red.
	
	selection all.
	
	selection applyAttribute:  color.
	"use start position because there are one span "
	self assert: (text startSpan next getAttribute: TxBackColorAttribute) equals: Color red.
	