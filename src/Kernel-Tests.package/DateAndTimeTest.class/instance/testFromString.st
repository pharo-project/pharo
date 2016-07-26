tests
testFromString

	| fromString fromStringNoOffset |
	fromString := DateAndTime fromString: '-1199-01-05T20:33:14.321-05:00'.
	self assert: (fromString printString = '-1199-01-05T20:33:14.321-05:00').
	
	"if no offset is provided, the local offset should be used"
	fromStringNoOffset := DateAndTime fromString: '-1199-01-05T20:33:14.321'.
	self assert: (fromStringNoOffset offset = DateAndTime localOffset).