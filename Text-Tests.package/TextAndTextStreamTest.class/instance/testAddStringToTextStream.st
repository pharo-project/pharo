tests
testAddStringToTextStream

	"It is possible to add a string into a TextStream.
	This test verifies that the created text has text attributes for all its characters. "

	| ts text |
	ts := TextStream on: (Text new: 50).
	ts nextPutAll: 'abc' asText.
	ts nextPutAll: 'def' asText allBold.
	ts nextPutAll: 'ghijk'.
	text := ts contents.
     " now, check the fundamental invariant of a text: "
	self assert: text string size = text runs size.
