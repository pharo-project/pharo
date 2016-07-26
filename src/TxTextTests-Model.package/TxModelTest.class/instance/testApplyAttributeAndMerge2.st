test-attribute
testApplyAttributeAndMerge2
	| color |
	self newText: 'AABBCC'.
	color := TxBackColorAttribute with: #something.
	selection beSelection end moveLeft: 2.
	selection start moveLeft: 4.
	
	selection applyAttribute:  color.
	self assert: text spans size = 5.  "<<  'AA' 'BB' 'CC'  >>"
	selection all.
	
	selection applyAttribute:  color.
	
	self assert: selection isValid.
	
	self assert: text spans size = 3. "<<  'AABBCC'  >>"
	cursor moveToStart.
	self assert: ((cursor  span style) get: TxBackColorAttribute) equals: #something.