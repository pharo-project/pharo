tests
testDoesNotUnderstand
	testedInstance content: ListPresenter.
	testedInstance items: {'aaa' . 'bbb' . 'ccc'}.
	self assertCollection: testedInstance listItems hasSameElements: {'aaa' . 'bbb' . 'ccc'}