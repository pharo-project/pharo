tests - copying same contents
testReverse

	| result |
	result := self nonEmpty reversed.	
	"verify content of 'result: '"
	1 to: result size do:
		[:i | self assert: ((result at: i) 
			= (self nonEmpty at: (self nonEmpty size - i + 1)))].
	"verify size of 'result' :"
	self assert: result size=self nonEmpty size.