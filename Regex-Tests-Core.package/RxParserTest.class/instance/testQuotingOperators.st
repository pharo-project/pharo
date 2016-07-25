tests
testQuotingOperators
	"self debug: #testQuotingOperators"
	
	"As we have seen, characters `*', `+', `?', `(', and `)' have special
meaning in regular expressions. If one of them is to be used
literally, it should be quoted: preceded with a backslash. (Thus,
backslash is also special character, and needs to be quoted for a
literal match--as well as any other special character described
further)."

	self deny: ('ab*' matchesRegex: 'ab*'). "	-- false: star in the right string is special"
	self assert: ('ab*' matchesRegex: 'ab\*').	 		
	self assert: ('a\c' matchesRegex: 'a\\c').		 	