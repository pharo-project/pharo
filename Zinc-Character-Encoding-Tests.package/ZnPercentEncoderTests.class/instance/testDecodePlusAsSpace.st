testing
testDecodePlusAsSpace
	| encoder |
	encoder := ZnPercentEncoder new.
	self assert: (encoder decode: '+') equals: ' '.
	self assert: encoder decodePlusAsSpace.
	encoder decodePlusAsSpace: false.
	self assert: (encoder decode: '+') equals: '+'.
	self deny: encoder decodePlusAsSpace