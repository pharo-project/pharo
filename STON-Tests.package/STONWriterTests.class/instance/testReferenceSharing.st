tests
testReferenceSharing
	| array one |
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self assert: (self serialize: array) = '[[#one],@2,@2]'.