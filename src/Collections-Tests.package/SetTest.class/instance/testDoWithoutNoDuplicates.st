tests - iterate
testDoWithoutNoDuplicates
	"self debug: #testDoWithoutNoDuplicates"
	| res |
	res := self speciesClass new.  
	self collection do: [:each | res add: each] without: -2.
	self assert: res size = self doWithoutNumber.