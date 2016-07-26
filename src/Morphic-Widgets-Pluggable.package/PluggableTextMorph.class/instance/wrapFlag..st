accessing
wrapFlag: aBoolean
	self textMorph ifNil: [self setText: ''].
	textMorph wrapFlag: aBoolean