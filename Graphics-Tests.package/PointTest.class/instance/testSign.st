tests - testing
testSign

	self assert: (-3 @ -2) sign = ( -1@ -1).
	self assert: (-3 @ 2) sign = ( -1@ 1).
	self assert: (3 @ -2) sign = ( 1@ -1).
	self assert: (3 @ 2) sign = ( 1@ 1).