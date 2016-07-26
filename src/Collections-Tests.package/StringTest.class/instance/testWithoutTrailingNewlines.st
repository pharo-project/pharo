tests - copying
testWithoutTrailingNewlines
	self assert: ('123',String cr) withoutTrailingNewlines = '123'.
	self assert: ('123',String lf) withoutTrailingNewlines = '123'.
	self assert: ('123',String cr, String lf) withoutTrailingNewlines = '123'.