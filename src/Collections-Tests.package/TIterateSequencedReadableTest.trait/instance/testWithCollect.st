tests - iterate on sequenced reable collections
testWithCollect
	
	| result firstCollection secondCollection index  |

	firstCollection := self nonEmptyMoreThan1Element.
	secondCollection := firstCollection copy.
	index := 0.
	
	result := firstCollection  with: secondCollection collect:
		[:a :b |
		( index := index + 1).
		self assert: (firstCollection at: index) equals:  a. 
		self assert: (secondCollection at: index) equals:  b.
		b].
	
	1 to: result size do:[: i | self assert: (result at:i)= (secondCollection  at: i)].
	self assert: result size = secondCollection  size.