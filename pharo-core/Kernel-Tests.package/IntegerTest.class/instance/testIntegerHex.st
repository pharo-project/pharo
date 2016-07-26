tests - printing
testIntegerHex
	| result |
	result := 15 asInteger hex.
	self assert: result = '16rF'.
	result := 0 asInteger hex.
	self assert: result = '16r0'.
	result := 255 asInteger hex.
	self assert: result = '16rFF'.
	result := 90 asInteger hex.
	self assert: result = '16r5A'