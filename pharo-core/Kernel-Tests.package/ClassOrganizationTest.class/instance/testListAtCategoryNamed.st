tests
testListAtCategoryNamed
	| methods |

	methods := self organization listAtCategoryNamed: 'empty'.
	self assert: methods isEmpty.
		
	methods := self organization listAtCategoryNamed: 'one'.
	self assert: methods size = 1.
	self assert: methods first = #one.