tests
testTextAndRunArray
	| texts |
	texts := { 
		'Text!' asText. 
		(Text string: 'I am bold' attribute: TextEmphasis bold), ' and I am normal text'.
		Text new }.
	self serializeAndMaterialize: texts