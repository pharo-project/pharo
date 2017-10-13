testing
testArgumentAt
	| pragma |
	pragma := Pragma keyword: #value:value:value: arguments: #( 3 2 1 ).
	self assert: (pragma argumentAt: 1) = 3.
	self assert: (pragma argumentAt: 2) = 2.
	self assert: (pragma argumentAt: 3) = 1