tests
toDotestSpecialCharacterInSetRange
	"self debug: #testSpecialCharacterInSetRange"
	
	"Special characters within a set are `^', `-', and `]' that closes the
set. Below are the examples of how to literally use them in a set:
	[01^]		-- put the caret anywhere except the beginning
	[01-]		-- put the dash as the last character
	[]01]		-- put the closing bracket as the first character 
	[^]01]			(thus, empty and universal sets cannot be specified)"

	self assert: ('0' matchesRegex: '[01^]').
	
	self assert: ('0' matchesRegex: '[0-9]').	
	