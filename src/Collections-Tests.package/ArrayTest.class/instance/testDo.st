tests - iterate
testDo

	| res |
	res := OrderedCollection new.  
	self collection do: [:each | res add: each class].
	self assert: res asArray = self result.