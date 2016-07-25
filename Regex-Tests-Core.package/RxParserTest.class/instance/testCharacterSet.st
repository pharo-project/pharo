tests
testCharacterSet
	"self debug: #testCharacterSet"
	
	"So far, we have used only characters as the 'smallest' components of
regular expressions. There are other, more `interesting', components.
A character set is a string of characters enclosed in square
brackets. It matches any single character if it appears between the
brackets. For example, `[01]' matches either `0' or `1':"

	self assert: ('0' matchesRegex: '[01]').	 	
	self deny: ('3' matchesRegex: '[01]'). 	 
	self deny: ('11' matchesRegex: '[01]').	"-- false: a set matches only one character"
	self deny: ('01' matchesRegex: '[01]').
