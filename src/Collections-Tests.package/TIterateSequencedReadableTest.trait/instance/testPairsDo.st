tests - iterate on sequenced reable collections
testPairsDo
	| index |
	index:=1.
	
	self nonEmptyMoreThan1Element  pairsDo: 
		[:each1 :each2 | 
		self assert:(self nonEmptyMoreThan1Element at:index)=each1.
		self assert:(self nonEmptyMoreThan1Element at:(index+1))=each2.
		index:=index+2].
	
	self nonEmptyMoreThan1Element size odd
		ifTrue:[self assert: index=self nonEmptyMoreThan1Element size]
		ifFalse:[self assert: index=(self nonEmptyMoreThan1Element size+1)].