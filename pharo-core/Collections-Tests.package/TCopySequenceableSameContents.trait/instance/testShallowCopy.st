tests - copying same contents
testShallowCopy
	| result |
	result := self nonEmpty shallowCopy .
	
	"verify content of 'result: '"
	1 to: self nonEmpty size do:
		[:i | self assert: ((result at:i)=(self nonEmpty at:i))].
	"verify size of 'result' :"
	self assert: result size=self nonEmpty size.