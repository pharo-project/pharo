tests
testCreationTimeIsADateAndTimeInstance
	"While creation is the message sent to a directory entry, creation returns a DateAndTime object"

	| creation |
	creation := self entry creation.
	self assert: creation class = DateAndTime.
	