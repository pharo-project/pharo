printing
printProgress
	| string |
	string := '[',currentTest asString, '/', maxTest asString,']'.
	self stderr 
		startOfLine; right: (80 - string size); nextPutAll: string.