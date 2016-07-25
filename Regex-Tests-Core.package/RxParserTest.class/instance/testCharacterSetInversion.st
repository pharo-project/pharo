tests
testCharacterSetInversion
	"self debug: #testCharacterSetInversion"
	
	"If the first character after the opening bracket is `^', the set is
inverted: it matches any single character *not* appearing between the
brackets:"
	
	self deny: ('0' matchesRegex: '[^01]').		  	
	"0 appears in 01 so there is no match"
	
	self assert: ('3' matchesRegex: '[^01]').
	"3 is not in 01 so it matches"
	
	
	self deny: ('30' matchesRegex: '[^01]').		
	self deny: ('33333333333333333333333330' matchesRegex: '[^01]').	
	"there is one zero so it does not match"