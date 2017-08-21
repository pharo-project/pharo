tests
testDoesNotUnderstand
	testedInstance content: ListModel.
	testedInstance items: {'aaa' . 'bbb' . 'ccc'}.
	self assertCollection: testedInstance listItems hasSameElements: {'aaa' . 'bbb' . 'ccc'}