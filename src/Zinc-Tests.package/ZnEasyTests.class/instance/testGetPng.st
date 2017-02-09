testing
testGetPng
	| url result |
	url := 'http://pharo.org/files/pharo.png'.
	result := ZnEasy getPng: url.
	self assert: result isForm