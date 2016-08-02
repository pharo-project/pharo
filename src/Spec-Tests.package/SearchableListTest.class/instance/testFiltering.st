tests
testFiltering
	testedInstance listModel items: {'aa' . 'bb' . 'ab' . 'ba'}.
	testedInstance searchModel text: 'a'.
	self assert: testedInstance listModel listItems asArray equals: {'aa' . 'ab'}