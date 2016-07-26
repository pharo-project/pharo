tests
testModificationTimeIsADateAndTimeInstance
	"While modification is the message sent to a directory entry, modification returns a DateAndTime object"

	| modification |
	modification := self entry modification.
	self assert: modification class = DateAndTime.
	