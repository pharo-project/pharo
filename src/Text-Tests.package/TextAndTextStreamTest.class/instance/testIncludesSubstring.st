tests
testIncludesSubstring
	"test that Text correctly checks that the supplied substring is present (case sensitive)"
	self assert: ('testing this string' asText includesSubstring: 'ring').
	self assert: ('éèàôüößäóñíá' asText includesSubstring: '').
	self deny: ('éèàôüößäóñíá' asText includesSubstring: 'a').
	self assert: ('éèàôüößäóñíá' asText includesSubstring: 'ßä').
	self deny: ('kjdsnlksjdf' asText includesSubstring: 'K')