testing
testUTF8EncoderAuto
	| encoder inputString bytes outputString |
	encoder := ZnUTF8Encoder new.
	inputString := String withAll: ((1 to: 3072) collect: [ :each | Character value: each ]).
	bytes := self encodeString: inputString with: encoder. 
	outputString := self decodeBytes: bytes with: encoder.
	self assert: inputString = outputString 