test-attribute
testApplyAttributeAndMerge
	"When you apply an TxAttribute on a span and is neighboor have the same collection of attribute you have to merge them"
	|color |
	self newText: 'AAACCC'.
	color := TxBackColorAttribute with: #something.
	
	selection all. 
	selection end moveToStart moveRight:3.
	
	selection applyAttribute:  color.  " <<  AAA CCC  >>"
	self assert: text spans size = 4.

	selection start moveToEnd.
	selection applyAttribute: color.  "CCC is now colored"
	
	"text spans second attributes = text spans third attributes"
	self assert: text spans size = 3. " <<  AAACCC  >>"
	cursor moveToStart.
	self assert: ((cursor  span  style) get: TxBackColorAttribute) equals: #something.