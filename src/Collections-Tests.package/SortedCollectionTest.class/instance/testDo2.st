tests - iterate
testDo2

	| res |
	res := self speciesClass sortBlock: [:a :b | a name < b name]..  
	self collection do: [:each | res add: each class].
	self assert: res asArray = self result asArray. 
