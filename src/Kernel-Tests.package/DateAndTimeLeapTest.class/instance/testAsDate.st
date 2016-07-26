tests
testAsDate
	self assert: aDateAndTime asDate equals:  ('February 29, 2004' asDate translateTo: 2 hours).