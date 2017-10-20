tests
testFiltering
	testedInstance listPresenter items: {'aa' . 'bb' . 'ab' . 'ba'}.
	testedInstance searchPresenter text: 'a'.
	self assert: testedInstance listPresenter listItems asArray equals: {'aa' . 'ab'}