testing - converting
testSubstrings
	self assert: ('aba' substrings: 'bc') isArray.
	self assert: ('aba' substrings: 'bc') = #('a' 'a').
	self assert: ('abba' substrings: 'bc') = #('a' 'a').
	self assert: ('babbab' substrings: 'bc') = #('a' 'a').
	self assert: ('bbabbabb' substrings: 'bc') = #('a' 'a').