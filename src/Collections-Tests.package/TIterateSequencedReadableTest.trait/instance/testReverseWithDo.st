tests - iterate on sequenced reable collections
testReverseWithDo
	
	| firstCollection secondCollection index |

	firstCollection := self nonEmptyMoreThan1Element.
	secondCollection := firstCollection copy.
	index := firstCollection size.
	
	firstCollection  reverseWith: secondCollection do:
		[:a :b |
	
		self assert: (firstCollection at: index) equals:  a. 
		self assert: (secondCollection at: index) equals:  b.
			( index := index - 1).]
	
	
	