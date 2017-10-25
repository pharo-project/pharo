tests
testCanDeselectByClick
	self deny: testedInstance canDeselectByClick.
	testedInstance click.
	self assert: testedInstance state.
	testedInstance click.
	self assert: testedInstance state.
	testedInstance canDeselectByClick: true.
	testedInstance click.
	self deny: testedInstance state