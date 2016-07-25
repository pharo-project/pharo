tests
testSymbolAsString
	self assert: (self serializeJson: #foo) = '"foo"'.
	self assert: (self serializeJson: #'FOO') = '"FOO"'.
