tests
testIncludesSubstringCaseSensitive
	"test that Text correctly checks that the supplied substring is present,
	both case sensitive and case insensitive"
	self deny: ('test this string' asText includesSubstring: 'Ring' caseSensitive: true).
	self assert: ('test this string' asText includesSubstring: 'Ring' caseSensitive: false).
	self deny: ('123éàôüöß' asText includesSubstring: '' caseSensitive: false).
	self assert: ('123éàôüöß' asText includesSubstring: 'öß' caseSensitive: true).
	self assert: ('123éàôüöß' asText includesSubstring: 'ÀÔ' caseSensitive: false)