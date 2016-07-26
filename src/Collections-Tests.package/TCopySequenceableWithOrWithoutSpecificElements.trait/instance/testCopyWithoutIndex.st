tests - copying with or without
testCopyWithoutIndex
	| result index |
	index := self indexInNonEmpty .
	result := self nonEmpty copyWithoutIndex: index .
	
	"verify content of 'result:'"
	1 to: result size do:
		[:i | 
		i<( index ) ifTrue: [self assert: ((result at:i )= (self nonEmpty at:i))].
		i>=( index ) ifTrue: [self assert: (result at:i )= (self nonEmpty at:(i+1))]].
	
	"verify size of result : "
	self assert: result size=(self nonEmpty size -1).