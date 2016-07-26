tests - copying same contents
testShuffled
	| result |
	result := self nonEmpty shuffled .
	
	"verify content of 'result: '"
	result do: [:each | self assert: (self nonEmpty occurrencesOf: each)=(result occurrencesOf: each)].
	"verify size of 'result' :"
	self assert: result size=self nonEmpty size.