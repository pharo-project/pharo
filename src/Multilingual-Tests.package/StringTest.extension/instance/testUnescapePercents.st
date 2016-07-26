*Multilingual-Tests-OtherLanguages
testUnescapePercents
	self assert: '' urlDecoded = ''.
	self assert: 'x' urlDecoded = 'x'.

	self assert: '+' urlDecoded = ' '.
	self assert: 'x+' urlDecoded = 'x '.
	self assert: '+x' urlDecoded = ' x'.
	self assert: 'x+x' urlDecoded = 'x x'.

	self should: [ '%' urlDecoded ] raise: Error.
	self should: [ '%3' urlDecoded ] raise: Error.
	self assert: '%3C' urlDecoded = '<'.
	
	self assert: '%3Cx%3E4%3C%2Fx%3E' urlDecoded = '<x>4</x>'.
	
	self assert: '!@#$%25%5E&*()%7B%7D%5B%5D=:/;?+''%22' urlDecoded  = '!@#$%^&*(){}[]=:/;? ''"'.
	self assert: '!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%3B%3F%2B''%22' urlDecoded  = '!@#$%^&*(){}[]=:/;?+''"'.
	self assert: '%21@%23%24%25%5E%26*%28%29%7B%7D%5B%5D%3D%3A/%3B%3F+%27%22' urlDecoded = '!@#$%^&*(){}[]=:/;? ''"'